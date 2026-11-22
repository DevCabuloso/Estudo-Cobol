
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FUNCIONARIOS.
       AUTHOR. KARLOS EDUARDO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-FUNCIONARIOS
               ASSIGN TO "FUNCIONARIOS/FUNCIONARIOS.DATA"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD ARQUIVO-FUNCIONARIOS.
       01 LINHA-FUNCIONARIO PIC X(100).

       WORKING-STORAGE SECTION.

       01 DADOS-FUNCIONARIO.
           05 MATRICULA-FUNCIONARIO PIC X(5).
           05 PRIMEIRO-NOME         PIC X(15).
           05 ULTIMO-NOME           PIC X(10).
           05 GENERO                PIC X(1).

       01 CONTADORES.
           05 TOTAL-HOMENS          PIC 9(3) VALUE 0.
           05 TOTAL-MULHERES        PIC 9(3) VALUE 0.

       01 LEITURA-FINALIZADA        PIC X VALUE "N".
       01 WS-FILE-STATUS            PIC XX.

       PROCEDURE DIVISION.

       INICIO.
           DISPLAY "================================".
           DISPLAY "    CONTAGEM DE FUNCIONARIOS".
           DISPLAY "================================".

           OPEN INPUT ARQUIVO-FUNCIONARIOS.

           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "ERRO AO ABRIR ARQUIVO: "
                   WS-FILE-STATUS
               STOP RUN
           END-IF.

           PERFORM UNTIL LEITURA-FINALIZADA = "S"

               READ ARQUIVO-FUNCIONARIOS
                   AT END
                       MOVE "S" TO LEITURA-FINALIZADA
                   NOT AT END
                       MOVE SPACES TO DADOS-FUNCIONARIO

                       UNSTRING LINHA-FUNCIONARIO
                           DELIMITED BY ";"
                           INTO MATRICULA-FUNCIONARIO
                                PRIMEIRO-NOME
                                ULTIMO-NOME
                                GENERO
                       END-UNSTRING

                       DISPLAY "================================"
                       DISPLAY "MATRICULA: ["
                           MATRICULA-FUNCIONARIO "]"
                       DISPLAY "PRIMEIRO NOME: ["
                           FUNCTION TRIM(PRIMEIRO-NOME) "]"
                       DISPLAY "ULTIMO NOME: ["
                           FUNCTION TRIM(ULTIMO-NOME) "]"
                       DISPLAY "GENERO: [" GENERO "]"

                       IF GENERO = "M"
                           ADD 1 TO TOTAL-HOMENS
                       ELSE
                           IF GENERO = "F"
                               ADD 1 TO TOTAL-MULHERES
                           END-IF
                       END-IF

               END-READ

           END-PERFORM.

           CLOSE ARQUIVO-FUNCIONARIOS.

           DISPLAY "================================".
           DISPLAY "TOTAL DE HOMENS: " TOTAL-HOMENS.
           DISPLAY "TOTAL DE MULHERES: " TOTAL-MULHERES.
           DISPLAY "================================".

           STOP RUN.
