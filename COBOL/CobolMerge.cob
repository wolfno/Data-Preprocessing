       IDENTIFICATION DIVISION.
       PROGRAM-ID.     CobolMerge.
      *****************************************************************
      **   
      **  Zwei Kundendateien gleicher Struktur zusammenfügen
      **  und dabei ungültige Daten gesondert protokollieren.
      **
      *****************************************************************
      **  13.10.2025             Erstellername: NOAH WOLFAHRT
      *****************************************************************



       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT kunde1 ASSIGN TO
      -    "Kunde1.txt"
           FILE STATUS IS KUNDE1STATUS.
           SELECT kunde2 ASSIGN TO
      -    "Kunde2.txt"
           FILE STATUS IS KUNDE2STATUS.
           SELECT kundeout ASSIGN TO
      -     "MergeOut.txt"
           FILE STATUS IS KUNDEOUTSTATUS.
           SELECT logdatei ASSIGN TO
      -     "LogFile.txt"
           FILE STATUS IS LOGSTATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  kunde1.
       01  kunde1-zeile.
           05 kunden-id1           PIC 9(10).
           05 FILLER               PIC X(01).
           05 erstellzeit1         PIC 9(14).
           05 FILLER               PIC X(01).
           05 gueltig1             PIC 9(08).
           05 FILLER               PIC X(01).
           05 plz1                 PIC 9(05).
           05 FILLER               PIC X(01).
           05 wohnort1             PIC X(30).
           05 FILLER               PIC X(01).
           05 strasse1             PIC X(30).
           05 FILLER               PIC X(01).
           05 hausnr1              PIC X(05).
       FD  kunde2.
       01  kunde2-zeile.
           05 kunden-id2           PIC 9(10).
           05 FILLER               PIC X(01).
           05 erstellzeit2         PIC 9(14).
           05 FILLER               PIC X(01).
           05 gueltig2             PIC 9(08).
           05 FILLER               PIC X(01).
           05 plz2                 PIC 9(05).
           05 FILLER               PIC X(01).
           05 wohnort2             PIC X(30).
           05 FILLER               PIC X(01).
           05 strasse2             PIC X(30).
           05 FILLER               PIC X(01).
           05 hausnr2              PIC X(05).
       FD  kundeout.
       01  kundeoutzeile           PIC X(72).
       FD  logdatei.
       01  ereignis-meldung        PIC X(72).
       01  ereignis.   
           05 ereignis-feldname    PIC X(15).
           05 ereignis-feldwert    PIC 9(15).
           05 ereignis-filler      PIC X(05)     VALUE '    '.
           05 ereignis-pruefung    PIC X(30).
           05 ereignis-rest        PIC X(07).
           
       WORKING-STORAGE SECTION.

       01  KUNDE1STATUS            PIC X(02).
       01  KUNDE2STATUS            PIC X(02).
       01  KUNDEOUTSTATUS          PIC X(02).
       01  LOGSTATUS               PIC X(02).
       
      * Diese Hilfsvariablen werden für das sofortige Bearbeiten einer
      * Zeile verwendet, sowie zur Überprüfung der Sortierreihenfolge.
       01  aktuellezeile.
           05 kunden-id            PIC 9(10).
           05 FILLER               PIC X(01).
           05 erstellzeit.
              10 erstelljahr       PIC 9(04).
              10 erstellmonat      PIC 9(02).
              10 erstelltag        PIC 9(02).
              10 erstellstd        PIC 9(02).
              10 erstellmin        PIC 9(02).
              10 erstellsec        PIC 9(02).
           05 FILLER               PIC X(01).
           05 gueltig.
              10 gueltigjahr       PIC 9(04).
              10 gueltigmonat      PIC 9(02).
              10 gueltigtag        PIC 9(02).
           05 FILLER               PIC X(01).
           05 plz                  PIC 9(05).
           05 FILLER               PIC X(01).
           05 wohnort              PIC X(30).
           05 FILLER               PIC X(01).
           05 strasse              PIC X(30).
           05 FILLER               PIC X(01).
           05 hausnr               PIC X(05).
      
      * Diese Hilfsvariablen werden verwendet, um die Sortier-
      * reihenfolge zu überprüfen.      
       01  letztezeile.
           05 lkunden-id           PIC 9(10).
           05 FILLER               PIC X(01).
           05 lerstellzeit.
              10 lerstelljahr      PIC 9(04).
              10 lerstellmonat     PIC 9(02).
              10 lerstelltag       PIC 9(02).
              10 lerstellstd       PIC 9(02).
              10 lerstellmin       PIC 9(02).
              10 lerstellsec       PIC 9(02).
           05 FILLER               PIC X(01).
           05 lgueltig.
              10 lgueltigjahr      PIC 9(04).
              10 lgueltigmonat     PIC 9(02).
              10 lgueltigtag       PIC 9(02).
           05 FILLER               PIC X(01).
           05 lplz                 PIC 9(05).
           05 FILLER               PIC X(01).
           05 lwohnort             PIC X(30).
           05 FILLER               PIC X(01).
           05 lstrasse             PIC X(30).
           05 FILLER               PIC X(01).
           05 lhausnr              PIC X(05).
                  
      * Das Ergebnis der Überprüfung der korrekten Reihenfolge
      * wird in diesem Switch gespeichert.
       01  SORTIER-STATUS          PIC 9(01).
           88 SORT-1                             VALUE 1.
           88 SORT-2                             VALUE 2.
           
      * Für das Log-File soll am Programmbeginn und -ende die
      * aktuelle Zeit ausgegeben werden.      
       01  systemdatum.
           05 systemjahr            PIC 9(02).
           05 systemmonat           PIC 9(02).
           05 systemtag             PIC 9(02).
       01  systemzeit.
           05 systemstd             PIC 9(02).
           05 systemmin             PIC 9(02).
           05 systemsec             PIC 9(02).
           05 systemmsec            PIC 9(02).

      * Für die Fehlerausgabe soll immer die entsprechende Datei
      * ausgegeben werden. Außerdem ist diese Variable für die
      * Haupt-Programmlogik wichtig.
       01  aktuelledatei           PIC 9(01)     VALUE 0.

      * Für die Fehlerausgabe soll immer die aktuelle Zeile in
      * der entsprechenden Datei ausgegeben werden. Am Ende wird
      * die Summe aller fehlerhaften Zeilen ausgegeben.
       01  zeilencounter1          PIC 9(05)     VALUE 0.
       01  zeilencounter2          PIC 9(05)     VALUE 0.
       01  zeilencountersumme      PIC 9(05)     VALUE 0.
       01  FEHLER-COUNTER          PIC 9(05)     VALUE 0.
       01  leere-zeilen-counter    PIC 9(05)     VALUE 0.

      * Dieser Switch ist für gravierende Fehler beim Öffnen, Lesen
      * oder Schreiben von Dateien, die zum Beenden des Programms 
      * führen.
       01  PROGRAMM-STATUS         PIC X(01).
           88 VERARBEITUNG-OK                    VALUE 'Y'.
           88 VERARBEITUNG-NICHT-OK              VALUE 'N'.

      * Dieser Switch wird aktiviert, wenn in einer Zeile
      * Fehler im Datenformat auftreten.
       01  ZEILE-STATUS            PIC X(01).
           88 ZEILE-OK                           VALUE 'Y'.
           88 ZEILE-NICHT-OK                     VALUE 'N'.

      * Diese Switches signalisieren, dass das Ende einer
      * Datei erreicht wurde.
       01  EOF-kunde1              PIC X(01)     VALUE 'N'.
           88 EOF1-YES                           VALUE 'Y'.
           88 EOF1-NO                            VALUE 'N'.
       01  EOF-kunde2              PIC X(01)     VALUE 'N'.
           88 EOF2-YES                           VALUE 'Y'.
           88 EOF2-NO                            VALUE 'N'.
      
      * Hilfsvariable, wird auf numerische Inhalte geprüft.
       01  hilfsnumeric            PIC 9(10).

      * Hilfsvariablen, die bei der Überprüfung von Datum
      * und Zeit relevant sind.
       01  hilfsdatum.
           05 hilfsjahr            PIC 9(04).
           05 hilfsmonat           PIC 9(02).
           05 hilfstag             PIC 9(02).
       01  hilfszeit.
           05 hilfsstd             PIC 9(02).
           05 hilfsmin             PIC 9(02).
           05 hilfssec             PIC 9(02).

      * Hilfsvariablen, die bei der Bestimmung von Schaltjahren
      * relevant sind. Müssen Rest nach Division durch 4, 100 und
      * 400 speichern.
       01  schaltjahrrest          PIC 9(03)     VALUE 1.
       01  schaltjahrrest2         PIC 9(03)     VALUE 1.
       01  schaltjahrrest3         PIC 9(03)     VALUE 1.
       01  schaltjahrfiller        PIC 9(04)     VALUE 0.
       01  schaltjahrfiller2       PIC 9(04)     VALUE 0.
       01  schaltjahrfiller3       PIC 9(04)     VALUE 0.


       PROCEDURE DIVISION.

       STEUERUNG SECTION.
       STEUERUNG-ANFANG.

           PERFORM VORLAUF

           IF VERARBEITUNG-OK
             PERFORM VERARBEITUNG
           END-IF

           PERFORM NACHLAUF

           .
       STEUERUNG-ENDE.
           STOP RUN.

      ******************************************************************
      *                                                                *
      *    Hier werden Anweisungen durchgeführt, die am Anfang         *
      *    von Bedeutung sind, wie das Öffnen von Dateien und          *
      *    die aktuelle Uhrzeit für die Log-Datei.                     *
      *                                                                *
      ******************************************************************
      
       VORLAUF SECTION.
       VORLAUF-ANFANG.

           SET VERARBEITUNG-OK TO TRUE
           ACCEPT systemdatum  FROM DATE
           ACCEPT systemzeit   FROM TIME

      * Programmbeginn kommunizieren
           DISPLAY SPACES
           DISPLAY "KundenAufgabeH wird gestartet."

      * Input- und Output-Dateien öffnen
           PERFORM FILEOPEN

      * Variablen initialisieren
           MOVE SPACES  TO ereignis
           MOVE SPACES  TO aktuellezeile
           SET ZEILE-OK TO TRUE
           SET EOF1-NO  TO TRUE
           SET EOF2-NO  TO TRUE

      * Log-Datei beschriften
           MOVE "Log-Datei für KundenaufgabeH" TO ereignis
           PERFORM LOGWRITE
           MOVE SPACES TO ereignis
           MOVE " Start des Programms: "           TO ereignis
           PERFORM SYSTEMZEITWRITE
           MOVE "******************************************" TO ereignis
           
           PERFORM LOGWRITE
           MOVE SPACES TO ereignis
           PERFORM LOGWRITE
           
           .
       VORLAUF-ENDE.
           EXIT.


      
      ******************************************************************
      *    Hier werden Routinen durchgeführt, die im Zusammenhang      *
      *    mit dem Vorlauf des Programms stehen. Dazu gehören das      *
      *    Öffnen von Dateien, das Schreiben der Log-Datei und         *
      *    der Output-Datei.                                           *
      ******************************************************************
      
       FILEOPEN SECTION.
       FILEOPEN-ANFANG.
      * Hier werden alle nötigen Dateien geöffnet und damit 
      * einhergehende Fehler bearbeitet.  
           
           OPEN INPUT  kunde1
           IF KUNDE1STATUS NOT = 00
              DISPLAY " ERROR   Kundendatei 1 konnte"
                      " nicht geoeffnet werden." KUNDE1STATUS
              SET VERARBEITUNG-NICHT-OK TO TRUE
           END-IF

           IF VERARBEITUNG-NICHT-OK
             GO TO FILEOPEN-ENDE
           END-IF

           OPEN INPUT  kunde2
           IF KUNDE2STATUS NOT = 00
              DISPLAY " ERROR   Kundendatei 2 konnte"
                      " nicht geoeffnet werden." KUNDE2STATUS
              SET VERARBEITUNG-NICHT-OK TO TRUE
           END-IF

           IF VERARBEITUNG-NICHT-OK
             GO TO FILEOPEN-ENDE
           END-IF

           OPEN OUTPUT kundeout
           IF KUNDEOUTSTATUS NOT = 00
              DISPLAY " ERROR   Output-Datei konnte"
                      " nicht geoeffnet werden." KUNDEOUTSTATUS
              SET VERARBEITUNG-NICHT-OK TO TRUE
           END-IF

           IF VERARBEITUNG-NICHT-OK
             GO TO FILEOPEN-ENDE
           END-IF

           OPEN OUTPUT logdatei
           IF LOGSTATUS NOT = 00
              DISPLAY " ERROR   Log-Datei konnte"
                      " nicht geoeffnet werden." LOGSTATUS
              SET VERARBEITUNG-NICHT-OK TO TRUE
           END-IF

           IF VERARBEITUNG-OK
              DISPLAY " SUCCESS Alle relevanten Dateien geoeffnet."
              DISPLAY SPACES
           END-IF

           .
       FILEOPEN-ENDE.
           EXIT.

       LOGWRITEMESSAGE SECTION.
       LOGWRITEMESSAGE-ANFANG.
      * Hier werden einleitende Informationen bei einem Fehler
      * in das Log-File ausgegeben: Dateinummer, Zeilennummer.

           IF LOGSTATUS NOT = 00
             SET VERARBEITUNG-NICHT-OK TO TRUE
             DISPLAY "Fehler vor dem Schreiben der Log-Datei." LOGSTATUS
             GO TO LOGWRITEMESSAGE-ENDE
           ELSE 
             WRITE ereignis-meldung
           END-IF
           
           IF LOGSTATUS NOT = 00
             SET VERARBEITUNG-NICHT-OK TO TRUE
             DISPLAY "Fehler beim Schreiben der Log-Datei." LOGSTATUS
             GO TO LOGWRITEMESSAGE-ENDE
           END-IF

           .
       LOGWRITEMESSAGE-ENDE.
           EXIT.

       LOGWRITE SECTION.
       LOGWRITE-ANFANG.
      * Hier werden genauere Fehlermeldungen in der Log-Datei
      * ausgegeben: Fehlerart, Fehlerprüfung, Fehlerwert.

           IF LOGSTATUS NOT = 00
             SET VERARBEITUNG-NICHT-OK TO TRUE
             DISPLAY "Fehler vor dem Schreiben der Log-Datei." LOGSTATUS
             GO TO LOGWRITE-ENDE
           ELSE 
             WRITE ereignis
           END-IF
           
           IF LOGSTATUS NOT = 00
             SET VERARBEITUNG-NICHT-OK TO TRUE
             DISPLAY "Fehler beim Schreiben der Log-Datei." LOGSTATUS
             GO TO LOGWRITE-ENDE
           END-IF

           .
       LOGWRITE-ENDE.
           EXIT.

       KUNDENOUTWRITE SECTION.
       KUNDENOUTWRITE-ANFANG.
      * Hier werden die Kunden-Output-Datei beschrieben und damit 
      * einhergehende Fehler bearbeitet.
           IF KUNDEOUTSTATUS NOT = 00
             SET VERARBEITUNG-NICHT-OK TO TRUE
             DISPLAY "Fehler vor dem Schreiben der "
                     "Kunden-Output-Datei." KUNDEOUTSTATUS
             SET VERARBEITUNG-NICHT-OK TO TRUE
           ELSE
             WRITE kundeoutzeile
           END-IF
           
           IF KUNDEOUTSTATUS NOT = 00
             SET VERARBEITUNG-NICHT-OK TO TRUE
             DISPLAY "Fehler beim Schreiben der "
                     "Kunden-Output-Datei." KUNDEOUTSTATUS
             SET VERARBEITUNG-NICHT-OK TO TRUE
           END-IF

           .
       KUNDENOUTWRITE-ENDE.
           EXIT.
           
       SYSTEMZEITWRITE SECTION.
       SYSTEMZEITWRITE-ANFANG.           
      * Hier wird die Log-Datei mit der aktuellen Systemzeit
      * beschrieben.
           MOVE systemtag                    TO ereignis (23:02)
           MOVE "."                          TO ereignis (25:01)
           MOVE systemmonat                  TO ereignis (26:02)
           MOVE "."                          TO ereignis (28:01)
           MOVE systemjahr                   TO ereignis (29:02)
           MOVE " um "                       TO ereignis (31:04)
           MOVE systemstd                    TO ereignis (35:02)
           MOVE ":"                          TO ereignis (37:01)
           MOVE systemmin                    TO ereignis (38:02)
           MOVE ":"                          TO ereignis (40:01)
           MOVE systemsec                    TO ereignis (41:02)
           MOVE " Uhr"                       TO ereignis (43:)
           PERFORM LOGWRITE

           MOVE SPACES TO ereignis
           PERFORM LOGWRITE
           
           .
       SYSTEMZEITWRITE-ENDE.



      ******************************************************************
      *                                                                *
      *    In dieser Section wird die wesentliche Programmlogik        *
      *    durchgeführt.                                               *
      *                                                                *
      ******************************************************************
       VERARBEITUNG SECTION.
       VERARBEITUNG-ANFANG.    
           
      * Jeweils erste Zeile einlesen
      * Wenn bereits das Ende der Datei erreicht wurde,
      * ist die Datei leer oder nur mit ungültigen Daten gefüllt.      
           PERFORM READKUNDE1
           IF VERARBEITUNG-NICHT-OK
             GO TO VERARBEITUNG-ENDE
           END-IF
           IF KUNDE1STATUS = 10
             MOVE "WARNUNG: Kundendatei 1 enthält keine gültigen Daten."
                  TO ereignis
             PERFORM LOGWRITE
             MOVE SPACES TO ereignis
             PERFORM LOGWRITE
           END-IF
           
           PERFORM READKUNDE2
           IF VERARBEITUNG-NICHT-OK
             GO TO VERARBEITUNG-ENDE
           END-IF
           IF KUNDE2STATUS = 10
             MOVE "WARNUNG: Kundendatei 2 enthält keine gültigen Daten."
                  TO ereignis
             PERFORM LOGWRITE
             MOVE SPACES TO ereignis
             PERFORM LOGWRITE
           END-IF
           
      * Hauptlogik der Zeilenausgabe
      
      * Wir führen eine Fallunterscheidung durch, je nachdem,
      * welche Dateien schon am Ende angelangt sind.
      
           PERFORM UNTIL EOF1-YES AND EOF2-YES
      * Solange beide Dateien noch aktiv sind, müssen wir die
      * Reihenfolge überprüfen.
      
             IF EOF1-NO AND EOF2-NO
               MOVE kunde1-zeile TO aktuellezeile
               MOVE kunde2-zeile TO letztezeile
               PERFORM SORTIER-CHECK
               
               IF SORT-1
                 MOVE 1 to aktuelledatei
                 MOVE kunde1-zeile TO kundeoutzeile
                 PERFORM KUNDENOUTWRITE
                 PERFORM READKUNDE1
                 IF VERARBEITUNG-NICHT-OK
                   GO TO VERARBEITUNG-ENDE
                 END-IF
           
               ELSE
                 MOVE 2 to aktuelledatei
                 MOVE kunde2-zeile TO kundeoutzeile
                 PERFORM KUNDENOUTWRITE
                 PERFORM READKUNDE2
                 IF VERARBEITUNG-NICHT-OK
                   GO TO VERARBEITUNG-ENDE
                 END-IF
               END-IF
             END-IF
      
      * Wenn nur noch eine Datei aktiv ist, ist die Entscheidung
      * für eine Zeile trivialer.      
             IF EOF1-YES
               MOVE 2 to aktuelledatei
               MOVE kunde2-zeile TO kundeoutzeile
               PERFORM KUNDENOUTWRITE
               IF EOF2-NO
                 PERFORM READKUNDE2
               END-IF
             END-IF
             
             IF EOF2-YES
               MOVE 1 to aktuelledatei
               MOVE kunde1-zeile TO kundeoutzeile
               PERFORM KUNDENOUTWRITE
               IF EOF1-NO
                 PERFORM READKUNDE1
               END-IF
             END-IF
                          
           END-PERFORM
           
           .
       VERARBEITUNG-ENDE.
           EXIT.



      ******************************************************************
      *                                                                *
      *    Hier werden die Kundendateien gelesen und                   *
      *    damit einhergehende Fehler bearbeitet.                      *
      *                                                                *
      ******************************************************************
      
       READKUNDE1 SECTION.
       READKUNDE1-ANFANG.
      * Lies die nächste korrekte Zeile von kunde1.
      * Erhöhe den Zeilencounter, überprüfe, ob die Reihenfolge stimmt,
      * und ob die Zeilen dem erwarteten Format entsprechen.
      * Prüfe, ob das Dateiende erreicht wurde.

      * Die "alte" Zeile wird für den Sortiercheck übergeben,
      * bevor die neue eingelesen wird.
           MOVE kunde1-zeile TO letztezeile
           
           IF KUNDE1STATUS = 00
             READ kunde1
             MOVE 1 TO aktuelledatei
             ADD 1 TO zeilencounter1
           END-IF     

           IF KUNDE1STATUS = 10
             SUBTRACT 1 FROM zeilencounter1
             SET EOF1-YES TO TRUE
             DISPLAY " Ende der Kundendatei 1 erreicht."
           ELSE
             IF KUNDE1STATUS = 00
               IF kunde1-zeile = SPACES
                 ADD 1 TO leere-zeilen-counter
                 MOVE SPACES        TO ereignis
                 MOVE "Zeile ist leer. Nächste Zeile lesen ..." 
                                    TO ereignis
                 PERFORM DATENFEHLER-LOGGEN
                 GO TO READKUNDE1-ANFANG
               END-IF
               MOVE kunde1-zeile TO aktuellezeile
               PERFORM ZEILEN-CHECK
               IF ZEILE-NICHT-OK
                 GO TO READKUNDE1-ANFANG
               ELSE
                 IF zeilencounter1 > 1
                   PERFORM SORTIER-CHECK
                   IF SORT-1
                     MOVE SPACES             TO ereignis
                     MOVE "Sortierfehler. Datei 1, Zeile " 
                                             TO ereignis (01:30)
                     MOVE zeilencounter1     TO ereignis (32:05)
                     MOVE "."                TO ereignis (37:)
                     PERFORM DATENFEHLER-LOGGEN
                     SET VERARBEITUNG-NICHT-OK TO TRUE                     
                     DISPLAY SPACES
                     DISPLAY " ERROR SORTIERFEHLER."
                     DISPLAY " Datei 1, Zeile " zeilencounter1
                     DISPLAY " Dateistatus " KUNDE1STATUS
                     DISPLAY SPACES
                     GO TO READKUNDE1-ENDE
                   END-IF
               END-IF
             ELSE
               DISPLAY "Fehler beim Lesen der "
                         "Kundendatei 1." KUNDE1STATUS
               SET VERARBEITUNG-NICHT-OK TO TRUE               
             END-IF               
           END-IF

           .
       READKUNDE1-ENDE.
           EXIT.

       READKUNDE2 SECTION.
       READKUNDE2-ANFANG.
      * Lies die nächste korrekte Zeile von kunde2.
      * Erhöhe den Zeilencounter, überprüfe, ob die Reihenfolge stimmt,
      * und gib im Falle Fehler aus, falls die Zeilen nicht dem
      * erwarteten Format entsprechen. Prüfe, ob das Dateiende
      * erreicht wurde.  
           
      * Die alte Zeile wird für den Sortiercheck übergeben,
      * bevor die neue eingelesen wird.
           MOVE kunde2-zeile TO letztezeile
           
           IF KUNDE2STATUS = 00
             READ kunde2
             MOVE 2 TO aktuelledatei
             ADD 1 TO zeilencounter2
           END-IF

           IF KUNDE2STATUS = 10
             SET EOF2-YES TO TRUE
             SUBTRACT 1 FROM zeilencounter2
             DISPLAY " Ende der Kundendatei 2 erreicht."
           ELSE
             IF KUNDE2STATUS = 00
               IF kunde2-zeile = SPACES
                 ADD 1 TO leere-zeilen-counter
                 MOVE SPACES        TO ereignis
                 MOVE "Zeile ist leer. Nächste Zeile lesen ..." 
                                    TO ereignis
                 PERFORM DATENFEHLER-LOGGEN
                 GO TO READKUNDE2-ANFANG
               END-IF
               MOVE kunde2-zeile TO aktuellezeile
               PERFORM ZEILEN-CHECK
               IF ZEILE-NICHT-OK
                 GO TO READKUNDE2-ANFANG
               ELSE
                 IF zeilencounter2 > 1
                   PERFORM SORTIER-CHECK
                   IF SORT-1
                     MOVE SPACES             TO ereignis
                     DISPLAY SPACES
                     MOVE "Sortierfehler. Datei 2, Zeile " 
                                             TO ereignis (01:30)
                     MOVE zeilencounter2     TO ereignis (32:)                
                     MOVE "."                TO ereignis (37:)
                     PERFORM DATENFEHLER-LOGGEN
                     SET VERARBEITUNG-NICHT-OK TO TRUE
                     DISPLAY " ERROR SORTIERFEHLER."
                     DISPLAY " Datei 2, Zeile " zeilencounter2
                     DISPLAY " Dateistatus " KUNDE2STATUS
                     DISPLAY SPACES
                     GO TO READKUNDE2-ENDE
                   END-IF
               END-IF
             ELSE
               DISPLAY "Fehler beim Lesen der "
                       "Kundendatei 2." KUNDE2STATUS
               SET VERARBEITUNG-NICHT-OK TO TRUE
             END-IF
           END-IF

           .
       READKUNDE2-ENDE.
           EXIT.

       DATENFEHLER-LOGGEN SECTION.
      * Erhöht den Fehlercounter, setzt den Switch auf ZEILE-NICHT-OK
      * und loggt das Fehler-Ereignis.
       DATENFEHLER-LOGGEN-ANFANG.
       
           ADD 1 TO FEHLER-COUNTER
           SET ZEILE-NICHT-OK TO TRUE      
           
           PERFORM LOGWRITE
                      
           MOVE SPACES             TO ereignis-meldung
           IF aktuelledatei = 1
             MOVE "Fehler: Kundendatei 1, Zeile "
                                   TO ereignis-meldung (01:29)
             MOVE zeilencounter1   TO ereignis-meldung (30:05)
             MOVE "."              TO ereignis-meldung (35:)
           ELSE
             MOVE "Fehler: Kundendatei 2, Zeile " 
                                   TO ereignis-meldung (01:29)
             MOVE zeilencounter2   TO ereignis-meldung (30:05)
             MOVE "."              TO ereignis-meldung (35:)
           END-IF
           
           PERFORM LOGWRITEMESSAGE
           
           MOVE SPACES TO ereignis
           PERFORM LOGWRITE
           
           .
       DATENFEHLER-LOGGEN-ENDE.
           EXIT.

      ******************************************************************
      *                                                                *
      *    Hier werden Routinen zum Überprüfen von                     *
      *    Input-Daten durchgeführt.                                   *
      *                                                                *
      ******************************************************************

       IS-NUMERIC-CHECK SECTION.
      * Überprüfe und gebe im Fall weiter an DATENFEHLER-LOGGEN.
       IS-NUMERIC-CHECK-ANFANG.
       
           IF hilfsnumeric IS NOT NUMERIC
             SET ZEILE-NICHT-OK TO TRUE
             MOVE "Numeric-Check" TO ereignis-pruefung
             PERFORM DATENFEHLER-LOGGEN
           END-IF
           
           .
       IS-NUMERIC-CHECK-ENDE.


       DATUM-CHECK SECTION.
      * Überprüfe und gebe im Fall weiter an DATENFEHLER-LOGGEN.
       DATUM-CHECK-ANFANG.
      * Überprüfe, ob numerische Daten übergeben wurden.
           MOVE ZERO       TO hilfsnumeric
           MOVE hilfsdatum TO hilfsnumeric (1:8)
           PERFORM IS-NUMERIC-CHECK
           IF ZEILE-NICHT-OK
             GO TO DATUM-CHECK-ENDE
           END-IF

      * Überprüfe, ob der Tag zum Monat passt.
           EVALUATE hilfsmonat 
             WHEN 1
             WHEN 3 
             WHEN 5 
             WHEN 7 
             WHEN 8 
             WHEN 10 
             WHEN 12
               IF hilfstag < 01 OR hilfstag > 31
                 MOVE "Tagprüfung" TO ereignis-pruefung
                 PERFORM DATENFEHLER-LOGGEN
               END-IF

             WHEN 4
             WHEN 6
             WHEN 9
             WHEN 11
               IF hilfstag < 01 OR hilfstag > 30
                 MOVE "Tagprüfung" TO ereignis-pruefung
                 PERFORM DATENFEHLER-LOGGEN
               END-IF

             WHEN 2
               IF hilfstag < 01 OR hilfstag > 29
                 MOVE "Tagprüfung" TO ereignis-pruefung
                 PERFORM DATENFEHLER-LOGGEN
               ELSE
                 IF hilfstag = 29
                   DIVIDE hilfsjahr BY 4 GIVING schaltjahrfiller
                   REMAINDER schaltjahrrest
                   DIVIDE hilfsjahr BY 100 GIVING schaltjahrfiller2
                   REMAINDER schaltjahrrest2
                   DIVIDE hilfsjahr BY 400 GIVING schaltjahrfiller3
                   REMAINDER schaltjahrrest3
                   IF (schaltjahrrest NOT = 0 OR schaltjahrrest2 = 0)
                      AND schaltjahrrest3 NOT = 0
                     MOVE "Schaltjahrfehler" TO ereignis-pruefung
                     PERFORM DATENFEHLER-LOGGEN 
                   END-IF
                 END-IF
               END-IF
             WHEN OTHER
               MOVE "Monatprüfung" TO ereignis-pruefung
               PERFORM DATENFEHLER-LOGGEN
           END-EVALUATE

           .
       DATUM-CHECK-ENDE.
           EXIT.

       ZEIT-CHECK SECTION.
      * Überprüfe und gebe im Fall weiter an DATENFEHLER-LOGGEN.
      * Akzeptiert werden Zeiten zwischen 00:00:00 und 23:59:59.
      * Insbesondere sind also Zeiten wie 24:00:00 o.Ä. ungültig.
      * Zeiten mit Schaltsekunden (23:59:60) sind ebenfalls ungültig.
       ZEIT-CHECK-ANFANG.
           MOVE ZERO       TO hilfsnumeric
           MOVE hilfszeit  TO hilfsnumeric (1:6)
           
           PERFORM IS-NUMERIC-CHECK
           IF ZEILE-NICHT-OK
             GO TO ZEIT-CHECK-ENDE
           END-IF
       
           IF NOT (hilfsstd >= 00 AND hilfsstd <= 23)
             MOVE "Uhrzeitprüfung Stunde" TO ereignis-pruefung
             PERFORM DATENFEHLER-LOGGEN
           ELSE
             IF NOT (hilfsmin >= 00 AND hilfsmin <= 59)
               MOVE "Uhrzeitprüfung Minute" TO ereignis-pruefung
               PERFORM DATENFEHLER-LOGGEN
             ELSE
               IF NOT (hilfssec >= 00 AND hilfssec <= 59)
               MOVE "Uhrzeitprüfung Sekunde" TO ereignis-pruefung
               PERFORM DATENFEHLER-LOGGEN
             END-IF
           END-IF
           
           .
       ZEIT-CHECK-ENDE.
           EXIT.

       ZEILEN-CHECK SECTION.
      * Setzt den Switch auf ZEILE-OK, falls die Daten in aktuellezeile
      * dem erwarteten Format entsprechen, und setzt den Switch auf 
      * ZEILE-NICHT-OK andernfalls.
       ZEILEN-CHECK-ANFANG.
           SET ZEILE-OK TO TRUE
           
      * Check: Ist die Kundennummer numerisch?
           MOVE ZERO TO hilfsnumeric
           MOVE kunden-id TO hilfsnumeric
           MOVE 'Kunden-ID: ' TO ereignis-feldname
           MOVE kunden-id TO ereignis-feldwert
           PERFORM IS-NUMERIC-CHECK

      * Check: Ist die PLZ numerisch?
           IF ZEILE-OK
             MOVE ZERO TO hilfsnumeric
             MOVE plz TO hilfsnumeric
             MOVE 'Postleitzahl: ' TO ereignis-feldname
             MOVE plz TO ereignis-feldwert
             PERFORM IS-NUMERIC-CHECK

      * Check: Ist das Gültigkeitsdatum korrekt?               
             IF ZEILE-OK
               MOVE ZERO TO hilfsdatum
               MOVE gueltig TO hilfsdatum
               MOVE 'Gültig ab: ' TO ereignis-feldname
               MOVE gueltig TO ereignis-feldwert
               PERFORM DATUM-CHECK
               
      * Check: Ist die Erstellzeit korrekt?
               IF ZEILE-OK
                 MOVE ZERO TO hilfsdatum
                 MOVE erstellzeit (01:08) TO hilfsdatum
                 MOVE ZERO TO hilfszeit
                 MOVE erstellzeit (09:06) TO hilfszeit
                 MOVE 'Erstellzeit: '     TO ereignis-feldname
                 MOVE erstellzeit         TO ereignis-feldwert
                 PERFORM DATUM-CHECK
                 PERFORM ZEIT-CHECK
               END-IF
             END-IF
           END-IF
            
           . 
       ZEILEN-CHECK-ENDE.
           EXIT.
         
       SORTIER-CHECK SECTION.
       SORTIER-CHECK-ANFANG.      
      * Setzt den Switch auf SORT-1, falls die Daten in "aktuellezeile"
      * entsprechend der Erwartung nach "letztezeile" gereiht werden 
      * sollen (ok), und setzt den Switch auf SORT-2 andernfalls.
      
           SET SORT-2 TO TRUE
           
           IF kunden-id < lkunden-id
             SET SORT-1 TO TRUE
           ELSE
             IF kunden-id = lkunden-id
               IF gueltig < lgueltig
                 SET SORT-1 TO TRUE
               ELSE
                 IF gueltig = lgueltig AND
                    erstellzeit > lerstellzeit
                   SET SORT-1 TO TRUE
                 END-IF
               END-IF
             END-IF
           END-IF
           
           .
       SORTIER-CHECK-ENDE.


      ******************************************************************
      *                                                                *
      *    Hier werden Anweisungen durchgeführt, die am Ende           *
      *    von Bedeutung sind, wie das Schließen von Dateien und       *
      *    die aktuelle Uhrzeit für die Log-Datei.                     *
      *                                                                *
      ******************************************************************

       NACHLAUF SECTION.
       NACHLAUF-ANFANG.

      * Anzahl verarbeiteter Zeilen loggen
           MOVE "******************************************" TO ereignis
           PERFORM LOGWRITE
           
           ADD zeilencounter1 TO zeilencounter2 GIVING
               zeilencountersumme
           
           MOVE SPACES TO ereignis
           MOVE " Anzahl gelesener Zeilen: " TO ereignis
           MOVE zeilencounter1               TO ereignis (27:05)
           MOVE " + "                        TO ereignis (32:03)
           MOVE zeilencounter2               TO ereignis (35:05)
           MOVE " = "                        TO ereignis (40:03)
           MOVE zeilencountersumme           TO ereignis (43:05)
           PERFORM LOGWRITE

      * Anzahl übersprungener Fehlerzeilen und leere Zeilen loggen
      * Die Daten werden nicht im Kundenoutput-File ausgegeben,
      * aber im Log-File detailliert angezeigt.
           MOVE SPACES TO ereignis
           MOVE " Anzahl der Zeilen mit Fehler: " TO ereignis (01:31)
           MOVE FEHLER-COUNTER                    TO ereignis (32:05)
           MOVE "."                               TO ereignis (37:)
           PERFORM LOGWRITE

           MOVE SPACES TO ereignis
           MOVE " Anzahl der leeren Zeilen: "     TO ereignis (01:27)
           MOVE leere-zeilen-counter              TO ereignis (28:05)
           MOVE "."                               TO ereignis (33:)
           PERFORM LOGWRITE

      * Programmende kommunizieren
           ACCEPT systemdatum  FROM DATE
           ACCEPT systemzeit   FROM TIME

           MOVE SPACES TO ereignis
           PERFORM LOGWRITE
          
           MOVE " Ende des Programms: " TO ereignis
           PERFORM SYSTEMZEITWRITE
           PERFORM LOGWRITE

      * Dateien schließen
      * TO DO Sollte noch expliziter und genauer gemacht werden,
      * damit man weiß, bei welcher Datei es zu einem Fehler kommt.
           IF VERARBEITUNG-OK AND 
             (KUNDE1STATUS NOT = 10 OR KUNDE2STATUS NOT = 10
              OR KUNDEOUTSTATUS NOT = 00 OR LOGSTATUS NOT = 00)
             DISPLAY " ERROR Fehler vor dem Beenden einer Datei."
             SET VERARBEITUNG-NICHT-OK TO TRUE
             GO TO NACHLAUF-ENDE
           ELSE
             CLOSE kunde1 kunde2 kundeout logdatei
           END-IF
                 
      * TO DO Sollte noch expliziter und genauer gemacht werden,
      * damit man weiß, bei welcher Datei es zu einem Fehler kommt.
           IF KUNDE1STATUS NOT = 00 OR KUNDE2STATUS NOT = 00
              OR KUNDEOUTSTATUS NOT = 00 OR LOGSTATUS NOT = 00
             DISPLAY "Fehler beim Beenden von mind. einer Datei."
             SET VERARBEITUNG-NICHT-OK TO TRUE
             GO TO NACHLAUF-ENDE
           ELSE
             DISPLAY SPACES
             DISPLAY " SUCCESS Alle relevanten Dateien geschlossen."              
           END-IF

      * Programmende kommunizieren
           DISPLAY "KundenAufgabeH wird beendet."

           .
       NACHLAUF-ENDE.
           EXIT.

