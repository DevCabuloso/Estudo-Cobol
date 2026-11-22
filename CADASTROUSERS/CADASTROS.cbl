       >>SOURCE FORMAT FREE
            
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADASTROS.
       AUTHOR. KARLOS EDUARDO.
            
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ARQUIVO-USUARIOS
                    ASSIGN TO "CADASTROUSERS/USUARIOS.DATA"
                    ORGANIZATION IS LINE SEQUENTIAL
                    FILE STATUS IS WS-FILE-STATUS.
            
       DATA DIVISION.
       FILE SECTION.
            
            FD ARQUIVO-USUARIOS.
       01 LINHA-USUARIO PIC X(100).
            
       WORKING-STORAGE SECTION.
            
       01 DADOS-USUARIO.
                05 AGENCIA             PIC X(5).
                05 NOME-USUARIO        PIC X(15).
                05 SOBRENOME-USUARIO   PIC X(20).
                05 SEXO-USUARIO        PIC X(1).
                05 TIPO-CONTA          PIC X(1).
       01 WS-CADASTRO-VALIDO PIC X VALUE "S".
            
       01 WS-AGENCIA-CONSULTA     PIC X(5).
       01 WS-USUARIO-ENCONTRADO   PIC X VALUE "N".
       01 WS-FIM-CONSULTA         PIC X VALUE "N".
            
       01 CONTADORES.
                05 TOTAL-CORRENTE      PIC 9(3) VALUE 0.
                05 TOTAL-POUPANCA      PIC 9(3) VALUE 0.
            
       01 LEITURA-FINALIZADA      PIC X VALUE "N".
       01 WS-FILE-STATUS         PIC XX.
       01 OPCAO                  PIC 9 VALUE 0.
            
            PROCEDURE DIVISION.
            
       INICIO.
            
                PERFORM UNTIL OPCAO = 4
            
                    DISPLAY "================================"
                    DISPLAY " CADASTRO E CONSULTA DE USUARIO "
                    DISPLAY "================================"
                    DISPLAY SPACE
                    DISPLAY "1 - CADASTRAR USER"
                    DISPLAY "2 - CONSULTAR USER"
                    DISPLAY "3 - LISTAR USER"
                    DISPLAY "4 - SAIR"
                    DISPLAY SPACE
                    DISPLAY "ESCOLHA UMA OPCAO:"
            
                    ACCEPT OPCAO
            
                    EVALUATE OPCAO
            
                        WHEN 1
                            PERFORM CADASTRAR
            
                        WHEN 2
                            PERFORM CONSULTAR
            
                        WHEN 3
                            PERFORM LISTAR
            
                        WHEN 4
                            DISPLAY "ENCERRANDO SISTEMA"
            
                        WHEN OTHER
                            DISPLAY "OPCAO INVALIDA"
            
                    END-EVALUATE
            
                END-PERFORM
            
                STOP RUN.
            
            
            *> =================================
            *> MODULO DE CADASTRO
            *> =================================
            
       CADASTRAR.
            
                DISPLAY "================================"
                DISPLAY " CADASTRO "
                DISPLAY "================================"
            
                MOVE SPACES TO DADOS-USUARIO
            
                DISPLAY "INFORME A AGENCIA:"
                ACCEPT AGENCIA
            
                DISPLAY "INFORME O NOME:"
                ACCEPT NOME-USUARIO
            
                DISPLAY "INFORME O SOBRENOME:"
                ACCEPT SOBRENOME-USUARIO
            
                DISPLAY "INFORME O SEXO (M/F):"
                ACCEPT SEXO-USUARIO
            
                DISPLAY "TIPO DE CONTA (C/P):"
                ACCEPT TIPO-CONTA
            
                *> NORMALIZA OS CAMPOS
                MOVE FUNCTION UPPER-CASE(SEXO-USUARIO)
                    TO SEXO-USUARIO
            
                MOVE FUNCTION UPPER-CASE(TIPO-CONTA)
                    TO TIPO-CONTA

                MOVE "S" TO WS-CADASTRO-VALIDO
            
                *> VALIDACAO DOS CAMPOS
                IF AGENCIA = SPACES
                    OR NOME-USUARIO = SPACES
                    OR SOBRENOME-USUARIO = SPACES
                    OR SEXO-USUARIO = SPACE
                    OR TIPO-CONTA = SPACE
            
                    DISPLAY "ERRO: PREENCHA TODOS OS CAMPOS."
                    EXIT PARAGRAPH
            
                END-IF
            
                *> VALIDACAO DO SEXO
                IF SEXO-USUARIO NOT = "M"
                    AND SEXO-USUARIO NOT = "F"
            
                    DISPLAY "SEXO INVALIDO."
                    EXIT PARAGRAPH
            
                END-IF
            
                *> VALIDACAO DO TIPO DE CONTA
                IF TIPO-CONTA NOT = "C"
                    AND TIPO-CONTA NOT = "P"
            
                    DISPLAY "TIPO DE CONTA INVALIDO."
                    EXIT PARAGRAPH
            
                END-IF
            
                *> MONTA A LINHA DO ARQUIVO
                MOVE SPACES TO LINHA-USUARIO
            
                STRING
                    FUNCTION TRIM(AGENCIA) DELIMITED BY SIZE
                    ";" DELIMITED BY SIZE
                    FUNCTION TRIM(NOME-USUARIO) DELIMITED BY SIZE
                    ";" DELIMITED BY SIZE
                    FUNCTION TRIM(SOBRENOME-USUARIO) DELIMITED BY SIZE
                    ";" DELIMITED BY SIZE
                    SEXO-USUARIO DELIMITED BY SIZE
                    ";" DELIMITED BY SIZE
                    TIPO-CONTA DELIMITED BY SIZE
                    INTO LINHA-USUARIO
                END-STRING
            
                *> ABRE O ARQUIVO PARA ADICIONAR REGISTROS
                OPEN EXTEND ARQUIVO-USUARIOS
            
                *> CRIA O ARQUIVO CASO NAO EXISTA
                IF WS-FILE-STATUS = "35"
            
                    OPEN OUTPUT ARQUIVO-USUARIOS
            
                END-IF
            
                *> VERIFICA A ABERTURA
                IF WS-FILE-STATUS NOT = "00"
            
                    DISPLAY "ERRO AO ABRIR ARQUIVO: "
                        WS-FILE-STATUS
            
                    EXIT PARAGRAPH
            
                END-IF
            
                *> GRAVA O USUARIO
                WRITE LINHA-USUARIO
            
                IF WS-FILE-STATUS = "00"
            
                    DISPLAY "USUARIO CADASTRADO COM SUCESSO!"
            
                ELSE
            
                    DISPLAY "ERRO AO GRAVAR USUARIO: "
                        WS-FILE-STATUS
            
                END-IF
            
                CLOSE ARQUIVO-USUARIOS
            
                EXIT PARAGRAPH.
            
            
            *> =================================
            *> MODULO DE CONSULTA
            *> =================================
            
       CONSULTAR.
            
                DISPLAY "================================"
                DISPLAY "       CONSULTA DE USUARIO      "
                DISPLAY "================================"
            
                *> RECEBE A AGENCIA PARA PESQUISA
                DISPLAY "INFORME A AGENCIA:"
                ACCEPT WS-AGENCIA-CONSULTA
            
                *> VERIFICA SE O CAMPO FOI PREENCHIDO
                IF WS-AGENCIA-CONSULTA = SPACES
            
                    DISPLAY "INFORME UMA AGENCIA VALIDA."
                    EXIT PARAGRAPH
            
                END-IF
            
                *> INICIALIZA OS CONTROLES
                MOVE "N" TO WS-USUARIO-ENCONTRADO
                MOVE "N" TO WS-FIM-CONSULTA
            
                *> ABRE O ARQUIVO PARA LEITURA
                OPEN INPUT ARQUIVO-USUARIOS
            
                *> VERIFICA SE O ARQUIVO FOI ABERTO
                IF WS-FILE-STATUS NOT = "00"
            
                    IF WS-FILE-STATUS = "35"
            
                        DISPLAY "NAO EXISTEM USUARIOS CADASTRADOS."
            
                    ELSE
            
                        DISPLAY "ERRO AO ABRIR ARQUIVO: "
                            WS-FILE-STATUS
            
                    END-IF
            
                    EXIT PARAGRAPH
            
                END-IF
            
                *> PERCORRE OS REGISTROS
                PERFORM UNTIL WS-FIM-CONSULTA = "S"
            
                    READ ARQUIVO-USUARIOS
            
                        AT END
            
                            MOVE "S" TO WS-FIM-CONSULTA
            
                        NOT AT END
            
                            *> LIMPA OS CAMPOS ANTERIORES
                            MOVE SPACES TO DADOS-USUARIO
            
                            *> SEPARA OS DADOS DO REGISTRO
                            UNSTRING LINHA-USUARIO
                                DELIMITED BY ";"
                                INTO AGENCIA
                                     NOME-USUARIO
                                     SOBRENOME-USUARIO
                                     SEXO-USUARIO
                                     TIPO-CONTA
                            END-UNSTRING
            
                            *> COMPARA A AGENCIA
                            IF AGENCIA = WS-AGENCIA-CONSULTA
            
                                DISPLAY "=============================="
                                DISPLAY "     USUARIO ENCONTRADO       "
                                DISPLAY "=============================="
            
                                DISPLAY "AGENCIA: "
                                    FUNCTION TRIM(AGENCIA)
            
                                DISPLAY "NOME: "
                                    FUNCTION TRIM(NOME-USUARIO)
            
                                DISPLAY "SOBRENOME: "
                                    FUNCTION TRIM(SOBRENOME-USUARIO)
            
                                DISPLAY "SEXO: "
                                    SEXO-USUARIO
            
                                DISPLAY "TIPO CONTA: "
                                    TIPO-CONTA
            
                                MOVE "S" TO WS-USUARIO-ENCONTRADO
                                MOVE "S" TO WS-FIM-CONSULTA
            
                            END-IF
            
                    END-READ
            
                END-PERFORM
            
                *> FECHA O ARQUIVO
                CLOSE ARQUIVO-USUARIOS
            
                *> VERIFICA SE ENCONTROU O USUARIO
                IF WS-USUARIO-ENCONTRADO = "N"
            
                    DISPLAY "USUARIO NAO ENCONTRADO!"
            
                END-IF
            
                EXIT PARAGRAPH.
            
            
            *> =================================
            *> MODULO DE LISTAGEM
            *> =================================
            
       LISTAR.
            
                DISPLAY "================================"
                DISPLAY "       LISTAGEM DE USUARIOS     "
                DISPLAY "================================"
            
                PERFORM LER-USUARIOS
            
                EXIT PARAGRAPH.
            
            
            *> =================================
            *> LEITURA DOS USUARIOS
            *> =================================
            
       LER-USUARIOS.
            
                MOVE "N" TO LEITURA-FINALIZADA
            
                *> ABRE O ARQUIVO PARA LEITURA
                OPEN INPUT ARQUIVO-USUARIOS
            
                *> VERIFICA A ABERTURA
                IF WS-FILE-STATUS NOT = "00"
            
                    IF WS-FILE-STATUS = "35"
            
                        DISPLAY "NAO EXISTEM USUARIOS CADASTRADOS."
            
                    ELSE
            
                        DISPLAY "ERRO AO ABRIR ARQUIVO: "
                            WS-FILE-STATUS
            
                    END-IF
            
                    EXIT PARAGRAPH
            
                END-IF
            
                *> PERCORRE TODOS OS REGISTROS
                PERFORM UNTIL LEITURA-FINALIZADA = "S"
            
                    READ ARQUIVO-USUARIOS
            
                        AT END
            
                            MOVE "S" TO LEITURA-FINALIZADA
            
                        NOT AT END
            
                            MOVE SPACES TO DADOS-USUARIO
            
                            *> SEPARA OS DADOS DO REGISTRO
                            UNSTRING LINHA-USUARIO
                                DELIMITED BY ";"
                                INTO AGENCIA
                                     NOME-USUARIO
                                     SOBRENOME-USUARIO
                                     SEXO-USUARIO
                                     TIPO-CONTA
                            END-UNSTRING
            
                            *> EXIBE OS DADOS
                            DISPLAY "=============================="
            
                            DISPLAY "AGENCIA: "
                                FUNCTION TRIM(AGENCIA)
            
                            DISPLAY "NOME: "
                                FUNCTION TRIM(NOME-USUARIO)
            
                            DISPLAY "SOBRENOME: "
                                FUNCTION TRIM(SOBRENOME-USUARIO)
            
                            DISPLAY "SEXO: "
                                SEXO-USUARIO
            
                            DISPLAY "TIPO CONTA: "
                                TIPO-CONTA
            
                            DISPLAY "------------------------------"
            
                    END-READ
            
                END-PERFORM
            
                *> FECHA O ARQUIVO
                CLOSE ARQUIVO-USUARIOS
            
                EXIT PARAGRAPH.
