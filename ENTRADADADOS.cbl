
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENTRADADADOS.
       AUTHOR. KARLOS EDUARDO.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 NOME                         PIC A(020).

       01 DATA-ATUAL.
          05 ANO-ATUAL                 PIC 9(04).
          05 MES-ATUAL                 PIC 9(02).
          05 DIA-ATUAL                 PIC 9(02).

       PROCEDURE DIVISION.
       PRINCIPAL.

           DISPLAY "Entre com seu nome:".
           ACCEPT NOME.

           ACCEPT DATA-ATUAL FROM DATE YYYYMMDD.

           DISPLAY "Bem-vindo " NOME "!".
           DISPLAY "Sabia que hoje e "
               DIA-ATUAL "/" MES-ATUAL "/" ANO-ATUAL.

        STOP RUN.
       END PROGRAM ENTRADADADOS.
