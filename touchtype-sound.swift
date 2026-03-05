import AppKit
import Foundation

let sounds = ["Tink", "Pop", "Morse", "Blow", "Bottle", "Frog",
              "Glass", "Hero", "Ping", "Purr", "Submarine",
              "Funk", "Basso", "Sosumi"]
var cache: [String: NSSound] = [:]
for name in sounds {
    if let s = NSSound(named: name) {
        cache[name] = s
    }
}

setbuf(stdout, nil)
print("READY")

while let line = readLine() {
    let parts = line.split(separator: " ")
    let name = String(parts[0])
    let vol = parts.count > 1 ? Float(parts[1]) ?? 0.5 : 0.5
    if let s = cache[name] {
        s.stop()
        s.volume = vol
        s.play()
    }
}
