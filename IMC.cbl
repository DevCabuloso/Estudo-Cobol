
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IMC.
       AUTHOR. KARLOS EDUARDO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 ALTURA       PIC 9V99.
       01 PESO         PIC 999.
       01 IMC-TOTAL    PIC 999V99.
       01 IMC-EXIBICAO PIC ZZ9.99.

       PROCEDURE DIVISION.
       PRINCIPAL.
           DISPLAY "Digite sua altura (Em mt): ".
           ACCEPT ALTURA.

           DISPLAY "Digite seu peso (Em KG): ".
           ACCEPT PESO.

           COMPUTE IMC-TOTAL = PESO / (ALTURA ** 2).

           MOVE IMC-TOTAL TO IMC-EXIBICAO.

           DISPLAY "Seu IMC e: " IMC-EXIBICAO.

           IF IMC-TOTAL < 18.5
               DISPLAY "Voce esta abaixo do peso."
           ELSE IF IMC-TOTAL > 18.5 AND IMC-TOTAL < 25
               DISPLAY "Voce esta com o peso ideal."
           ELSE IF IMC-TOTAL > 30
               DISPLAY "Voce esta com sobrepeso."
           END-IF
           END-IF.

           STOP RUN.
       END PROGRAM IMC.

