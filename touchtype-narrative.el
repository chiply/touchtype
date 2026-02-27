;;; touchtype-narrative.el --- Narrative text mode for touchtype -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charlie Holland

;; Author: Charlie Holland <mister.chiply@gmail.com>
;; URL: https://github.com/chiply/touchtype

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Narrative typing mode that sources passages from Project Gutenberg
;; plain-text books.  Downloads are cached locally so only the first
;; use of each book requires network access.

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'touchtype-var)

;;;; Customization

(defconst touchtype-narrative--gutenberg-url-format
  "https://www.gutenberg.org/cache/epub/%d/pg%d.txt"
  "URL format for Gutenberg plain-text downloads.
Takes the book ID twice (for the directory and filename).")

(defcustom touchtype-narrative-book-list
  '(1342    ; Pride and Prejudice
    11      ; Alice's Adventures in Wonderland
    1661    ; Sherlock Holmes
    84      ; Frankenstein
    1952    ; The Yellow Wallpaper
    174     ; The Picture of Dorian Gray
    345     ; Dracula
    1080    ; A Modest Proposal
    16328   ; Beowulf
    98      ; A Tale of Two Cities
    1260    ; Jane Eyre
    219     ; Heart of Darkness
    2701    ; Moby Dick
    74      ; Tom Sawyer
    76      ; Huckleberry Finn
    120     ; Treasure Island
    35      ; The Time Machine
    5200    ; Metamorphosis
    1400    ; Great Expectations
    55)     ; The Wonderful Wizard of Oz
  "List of Project Gutenberg book IDs for narrative mode."
  :type '(repeat integer)
  :group 'touchtype)

(defcustom touchtype-narrative-cache-dir
  (expand-file-name "touchtype" user-emacs-directory)
  "Directory for caching downloaded Gutenberg texts."
  :type 'directory
  :group 'touchtype)

(defcustom touchtype-narrative-passage-chars 400
  "Approximate number of characters per narrative passage."
  :type 'integer
  :group 'touchtype)

;;;; Download and caching

(defun touchtype-narrative--cache-path (book-id)
  "Return the cache file path for BOOK-ID."
  (expand-file-name (format "%d.txt" book-id) touchtype-narrative-cache-dir))

(defun touchtype-narrative--retrieve (book-id)
  "Download Gutenberg book BOOK-ID and cache it.  Return the cache path.
If already cached, return the path immediately."
  (let ((path (touchtype-narrative--cache-path book-id)))
    (if (file-readable-p path)
        path
      (let* ((url (format touchtype-narrative--gutenberg-url-format book-id book-id))
             (buf (condition-case err
                      (url-retrieve-synchronously url t nil 30)
                    (error (user-error "Failed to download book %d: %s" book-id err)))))
        (unless buf
          (user-error "Failed to download book %d: no response" book-id))
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-min))
              ;; Strip HTTP headers
              (when (re-search-forward "\r?\n\r?\n" nil t)
                (delete-region (point-min) (point)))
              (unless (file-directory-p touchtype-narrative-cache-dir)
                (make-directory touchtype-narrative-cache-dir t))
              (write-region (point-min) (point-max) path nil 'quiet))
          (kill-buffer buf))
        path))))

;;;; Text extraction

(defun touchtype-narrative--extract-body (text)
  "Strip Gutenberg header/footer from TEXT, returning just the body."
  (let ((start 0)
        (end (length text)))
    (when (string-match "\\*\\*\\* ?START OF .+\\*\\*\\*" text)
      (setq start (match-end 0)))
    (when (string-match "\\*\\*\\* ?END OF .+\\*\\*\\*" text start)
      (setq end (match-beginning 0)))
    (string-trim (substring text start end))))

;;;; Passage extraction

(defun touchtype-narrative--random-passage (body)
  "Extract a passage of ~`touchtype-narrative-passage-chars' from BODY.
Finds a sentence boundary near a random position and reads forward."
  (let* ((len (length body))
         (target touchtype-narrative-passage-chars)
         (max-start (max 0 (- len target)))
         (pos (if (> max-start 0) (random max-start) 0)))
    ;; Find preceding sentence boundary (. ! ? followed by space/newline)
    (when (string-match ".*[.!?][ \n]" (substring body 0 pos))
      (setq pos (match-end 0)))
    ;; Read forward ~target chars
    (let ((end (min len (+ pos target))))
      ;; Extend to next sentence end
      (when (and (< end len)
                 (string-match "[.!?]" body end))
        (setq end (min len (1+ (match-beginning 0)))))
      ;; Clean: collapse whitespace to single spaces, trim
      (let ((raw (substring body pos end)))
        (setq raw (replace-regexp-in-string "[\n\r\t]+" " " raw))
        (setq raw (replace-regexp-in-string "  +" " " raw))
        (string-trim raw)))))

;;;; Passage preparation

(defun touchtype-narrative--prepare-passage ()
  "Download a random book and extract a passage into buffer-local vars."
  (let* ((book-id (nth (random (length touchtype-narrative-book-list))
                       touchtype-narrative-book-list))
         (path (touchtype-narrative--retrieve book-id))
         (text (with-temp-buffer
                 (insert-file-contents path)
                 (buffer-string)))
         (body (touchtype-narrative--extract-body text))
         (passage (touchtype-narrative--random-passage body)))
    (setq touchtype--narrative-passage passage)
    (setq touchtype--narrative-offset 0)
    passage))

;;;; Line generation

(defun touchtype-narrative-generate-line ()
  "Generate a line from the current narrative passage.
Reads from `touchtype--narrative-passage' at `touchtype--narrative-offset',
breaking at ~60 chars on a word boundary.  Returns nil when the passage
is exhausted."
  (when (and touchtype--narrative-passage
             (< touchtype--narrative-offset
                (length touchtype--narrative-passage)))
    (let* ((passage touchtype--narrative-passage)
           (offset  touchtype--narrative-offset)
           (remaining (- (length passage) offset))
           (target 60))
      (if (<= remaining target)
          ;; Take whatever's left
          (prog1 (string-trim (substring passage offset))
            (setq touchtype--narrative-offset (length passage)))
        ;; Find a word boundary near target
        (let ((break-pos (+ offset target)))
          ;; Search backward for a space
          (while (and (> break-pos offset)
                      (not (= (aref passage break-pos) ?\s)))
            (cl-decf break-pos))
          ;; If no space found, search forward instead
          (when (= break-pos offset)
            (setq break-pos (+ offset target))
            (while (and (< break-pos (length passage))
                        (not (= (aref passage break-pos) ?\s)))
              (cl-incf break-pos)))
          (let ((line (string-trim (substring passage offset break-pos))))
            (setq touchtype--narrative-offset
                  (min (length passage) (1+ break-pos)))
            line))))))

(provide 'touchtype-narrative)

;;; touchtype-narrative.el ends here
