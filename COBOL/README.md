# CobolMerge


Dieses Repository enthält ein COBOL-Programm, das zwei Adressdateien zusammenführt. Die Eingabedateien Kunde1.txt und Kunde2.txt wurden KI-generiert und enthalten fiktive Daten. Sie haben denselben Aufbau und beinhalten die folgenden Felder:

* Kundennummer (zehnstellig)

* Timestamp (Erstelldatum/-zeit des Datensatzes im Format JJJJMMTTHHMMSS)

* GLTGAB (Gültig ab, Datum im Format JJJJMMTT)

* Postleitzahl (fünfstellig)

* Ort (dreißigstellig)

* Straße (dreißigstellig)

* Hausnummer (fünfstellig)

Beide Eingabedateien sind nach den folgenden Kriterien sortiert:

* Kundennummer (aufsteigend)

* GLTGAB (aufsteigend)

* Timestamp (absteigend)

<br> 

## Verarbeitung:

Das Programm liest die beiden Dateien ein, überprüft die Korrektheit der Daten und führt sie zusammen. Dabei wird die Sortierreihenfolge beibehalten.

Prüfung der Eingabedaten:

* Kundennummer: Muss numerisch sein.

* Postleitzahl: Muss numerisch sein.

* GLTGAB: Muss ein gültiges Datum sein.

* Timestamp: Muss sowohl ein gültiges Datum als auch eine gültige Uhrzeit enthalten.

Ungültige oder fehlerhafte Datensätze werden im Log protokolliert und übersprungen.

Es wird geprüft, ob die Eingabedaten tatsächlich der erwarteten Sortierreihenfolge entsprechen. Wenn eine Abweichung festgestellt wird, bricht das Programm ab und protokolliert den Fehler.

<br> 

## Ausgabedatei:

Eine Ausgabedatei MergeOut.txt wird erstellt, die das zusammengeführte Ergebnis in demselben Format wie die Eingabedateien enthält.

Zusätzlich wird eine Protokolldatei LogFile.txt erstellt, die folgende Informationen enthält:

* Start- und Endzeitpunkt des Programmlaufs

* Anzahl der verarbeiteten Datensätze in den Eingabe- und Ausgabedateien

* Protokollierung von Fehlern und ungültigen Datensätzen

<br> 

## Features:

* Datenvalidierung für die Felder Kundennummer, Postleitzahl, GLTGAB und Timestamp

* Beibehaltung der Sortierreihenfolge beim Zusammenführen der Dateien

* Protokollierung von Start- und Endzeiten sowie der Anzahl der verarbeiteten Datensätze

* Fehlerbehandlung bei ungültigen Datensätzen und bei Verstöße gegen die Sortierreihenfolge
