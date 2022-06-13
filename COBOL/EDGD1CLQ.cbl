      ******************************************************************
      * FECHA       : 08/06/2022                                       *
      * PROGRAMADOR : DENIS GOMEZ                                      *
      * APLICACION  : SEMILLERO                                        *
      * PROGRAMA    : EDGD1CLQ                                         *
      * TIPO        : EN LINEA                                         *
      * DESCRIPCION : PROGRAMA PARA BROWSER DE MAESTRO DE CREDITOS     *
      *             : EN MAPA AMPLIADO Y AIX                           *
      * ARCHIVOS    : EDAMCR                                           *
      * ACCION (ES) : C=CONSULTAR,                                     *
      * PROGRAMA(S) : XCTL - EDGD1CL3                                  *
      * CANAL       : ADMINISTRATIVA                                   *
      * INSTALADO   : 08/06/2022                                       *
      * BPM/RATIONAL:                                                  *
      * NOMBRE      : DON EDGAR - INSTRUCTOR                           *
      * DESCRIPCION : PROYECTO                                         *
      ******************************************************************
       ID DIVISION.
       PROGRAM-ID. EDGD1CLQ.
       AUTHOR. DENISGOM.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CURRENCY SIGN IS 'Q ' WITH PICTURE SYMBOL 'Q'.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       COPY EDCLQ.
       COPY DFHAID.
       COPY EDMACR.
       COPY DFHBMSCA.
       01 WK-CAMPOS-DE-TRABAJO.
           02 WK-NOMBRE-PROGRAMA        PIC X(8) VALUE 'EDGD1CLQ'.
           02 WK-SISTEMA.
               03 WK-FECHA-SISTEMA        PIC 9(08) VALUE ZERO.
               03 WK-HORA-SISTEMA.
                   04 WK-HH-SISTEMA       PIC 99.
                   04 WK-MM-SISTEMA       PIC 99.
           02 WK-PRN-HORA.
               03 WK-PRN-HH           PIC 99.
               03 FILLER              PIC X VALUE ':'.
               03 WK-PRN-MM           PIC 99.
           02 WK-COM.
               03 WK-PANTALLA-ACTUAL  PIC 99.
               03 WK-COUNT-PANTALLA   PIC 99.
               03 WK-LLAVES OCCURS 15.
                   04 WK-PRI-POS      PIC 9(8).
                   04 WK-ULT-POS      PIC 9(8).
                   04 WK-PRI-POS-CRE  PIC 9(12).
                   04 WK-ULT-POS-CRE  PIC 9(12).
               03 WK-SIGNO2           PIC X.
           02 WK-COM-ENVIAR.
               03 WK-PANTALLA-ACTUAL-EN  PIC 99.
               03 WK-SEL-ENVIAR       PIC 9(08) OCCURS 10.
               03 WK-SIGNO-EN         PIC X.
           02 WK-I                    PIC 99.
               88 WK-END-OF-I         VALUE 14.
           02 WK-J                    PIC 99.
           02 WK-FIN-DE-ARCHIVO       PIC X.
               88 WK-END-OF-FILE      VALUE HIGH-VALUE.
           02 WK-POS-CURSOR           PIC 9(04).
           02 WK-FILA-CURSOR          PIC 99.
           02 WK-REGISTRO             PIC S99.
           02 WK-LLAVE-AUX            PIC 9(8).
      
      
       LINKAGE SECTION.
       01 DFHCOMMAREA.
           02 LNK-LARGO   PIC X OCCURS 0 TO 600 DEPENDING ON EIBCALEN.
      
       PROCEDURE DIVISION.
       MIAN.
           PERFORM 050-OBTENER-FECHA.
           IF EIBCALEN NOT = 0
               MOVE DFHCOMMAREA TO WK-COM
           END-IF.
      *---------- EVALUAR TECLA PRESIONADA
           EVALUATE TRUE
               WHEN EIBCALEN = 0
                   PERFORM 000-MAPA-INICIAL
                   PERFORM 060-ENVIAR-DATOS
                   PERFORM 070-REGRESAR-TRAN
               WHEN EIBTRNID = 'EDC3'
                   MOVE DFHCOMMAREA TO WK-COM-ENVIAR
                   PERFORM 000-MAPA-INICIAL
                   SUBTRACT 1 FROM WK-PANTALLA-ACTUAL-EN
                   PERFORM WK-PANTALLA-ACTUAL-EN  TIMES
                           ADD 1 TO WK-COUNT-PANTALLA
                           PERFORM 250-PANTALLA-NUEVA
                           MOVE '-' TO  EDCLQ-SIGNO2O
                           MOVE SPACE TO EDCLQ-MENSAJEO
                           ADD 1 TO WK-PANTALLA-ACTUAL
                           PERFORM 250-GUARDAR-LLAVES
                   END-PERFORM
                   PERFORM 060-ENVIAR-DATOS
                   PERFORM 070-REGRESAR-TRAN
               WHEN EIBAID = DFHPF10
                   PERFORM 999-SALIR
               WHEN EIBAID = DFHENTER
                   PERFORM 100-RECIBIR-MAPA
                   PERFORM 500-BUSCAR-CURSOR
                   PERFORM 060-ENVIAR-DATOS
                   PERFORM 070-REGRESAR-TRAN
               WHEN EIBAID = DFHPF5
                   PERFORM 100-RECIBIR-MAPA
                   IF  EDCLQ-SIGNOI = '+'
                       IF (WK-PANTALLA-ACTUAL + 1) > WK-COUNT-PANTALLA
                           ADD 1 TO WK-COUNT-PANTALLA
                           PERFORM 250-PANTALLA-NUEVA
                           MOVE '-' TO  EDCLQ-SIGNO2O
                           MOVE SPACE TO EDCLQ-MENSAJEO
                           ADD 1 TO WK-PANTALLA-ACTUAL
                           PERFORM 250-GUARDAR-LLAVES
                       ELSE
                           ADD 1 TO WK-PANTALLA-ACTUAL
                           PERFORM 300-CARGAR-PANTALLA
                           MOVE '-' TO  EDCLQ-SIGNO2O
                       END-IF
                       MOVE SPACE
                       TO EDCLQ-MENSAJEO
                   ELSE
                       MOVE 'No hay mas registro hacia abajo'
                       TO EDCLQ-MENSAJEO
                   END-IF
      
                   PERFORM 060-ENVIAR-DATOS
                   PERFORM 070-REGRESAR-TRAN
               WHEN EIBAID = DFHPF6
                   PERFORM 100-RECIBIR-MAPA
                   MOVE SPACE TO EDCLQ-MENSAJEO
                   IF  EDCLQ-SIGNO2I = '-'
                       SUBTRACT 1 FROM WK-PANTALLA-ACTUAL
                       PERFORM 300-CARGAR-PANTALLA
                       MOVE '+' TO  EDCLQ-SIGNOO
                       IF WK-PANTALLA-ACTUAL = 1
                           MOVE SPACE TO EDCLQ-SIGNO2I
                       END-IF
                   ELSE
                       MOVE 'No hay mas registro hacia arriba'
                       TO EDCLQ-MENSAJEO
                   END-IF
                   PERFORM 060-ENVIAR-DATOS
                   PERFORM 070-REGRESAR-TRAN
               WHEN EIBAID = DFHPF7
                   PERFORM 100-RECIBIR-MAPA
                   PERFORM 400-BUSCAR-CLIENTE
               WHEN OTHER
                   MOVE 'Tecla Invalida' TO EDCLQ-MENSAJEO
                   PERFORM 060-ENVIAR-DATOS
                   PERFORM 070-REGRESAR-TRAN
           END-EVALUATE.
      
       000-MAPA-INICIAL.
           EXEC CICS
                SEND MAP('EDCLQ')
                MAPSET('EDCLQ')
                ERASE
                ALTERNATE
           END-EXEC.
           MOVE 0 TO EDMP-NUMERO-PRESTAMO.
           MOVE 0 TO WK-LLAVE-AUX
           MOVE 1 TO WK-COUNT-PANTALLA.
           MOVE 1 TO WK-PANTALLA-ACTUAL.
           PERFORM 200-CARGAR-PANTALLA-INICIAL.
           PERFORM 250-GUARDAR-LLAVES.
      
       050-OBTENER-FECHA.
           MOVE FUNCTION CURRENT-DATE(1:12) TO WK-SISTEMA.
           MOVE WK-FECHA-SISTEMA TO EDCLQ-FECHAO.
           MOVE WK-HH-SISTEMA TO WK-PRN-HH.
           MOVE WK-MM-SISTEMA TO WK-PRN-MM.
           MOVE WK-PRN-HORA TO EDCLQ-HORAO.
      
      *--------------- ENVIAR DATOS AL MAPA -------------*
       060-ENVIAR-DATOS.
           EXEC CICS
                SEND MAP('EDCLQ')
                MAPSET('EDCLQ')
                DATAONLY
                NOHANDLE
           END-EXEC.
      
      *------------- REGRESAR CONTROL A TRANSACCION -----------*
       070-REGRESAR-TRAN.
           EXEC CICS
                RETURN
                TRANSID('EDCQ')
                COMMAREA(WK-COM)
           END-EXEC.
      
      *------------ RECIBIR DATOS DEL MAPA -----------*
       100-RECIBIR-MAPA.
           EXEC CICS
                RECEIVE  MAP('EDCLQ')
                MAPSET('EDCLQ')
           END-EXEC.
      
      *--------------- PANTALLA INICIAL ---------------*
       200-CARGAR-PANTALLA-INICIAL.
           EXEC CICS
                STARTBR
                FILE('EDP3CR')
                RIDFLD(WK-LLAVE-AUX)
                GTEQ
                NOHANDLE
           END-EXEC.
           EVALUATE EIBRESP
               WHEN DFHRESP(NOTOPEN)
                   MOVE 'ARCHIVO NO ABIERTO' TO EDCLQ-MENSAJEO
               WHEN DFHRESP(NORMAL)
                   EXEC CICS
                        READNEXT
                        FILE('EDP3CR')
                        RIDFLD(WK-LLAVE-AUX)
                        INTO(REG-EDMACR)
                        NOHANDLE
                   END-EXEC
                   EVALUATE EIBRESP
                       WHEN DFHRESP(ENDFILE)
                           SET WK-END-OF-FILE TO TRUE
                       WHEN DFHRESP(DUPKEY)
                       WHEN DFHRESP(NORMAL)
                           PERFORM 220-MOVE-ADELANTE
                       WHEN OTHER
                           MOVE 'OCURRIO UN ERROR' TO EDCLQ-MENSAJEO
                   END-EVALUATE
               WHEN OTHER
                   MOVE 'OCURRIO UN ERROR START' TO EDCLQ-MENSAJEO
           END-EVALUATE.
      
      *--------------- LEER HACIA ADELANTE ---------------*
       250-PANTALLA-NUEVA.
           MOVE WK-ULT-POS(WK-PANTALLA-ACTUAL) TO WK-LLAVE-AUX
           EXEC CICS
                STARTBR
                FILE('EDP3CR')
                RIDFLD(WK-LLAVE-AUX)
                GTEQ
                NOHANDLE
           END-EXEC.
           EVALUATE EIBRESP
               WHEN DFHRESP(NOTOPEN)
                   MOVE 'ARCHIVO NO ABIERTO' TO EDCLQ-MENSAJEO
               WHEN DFHRESP(NORMAL)
                   PERFORM UNTIL EDMP-LLAVE =
                   WK-ULT-POS-CRE(WK-PANTALLA-ACTUAL) OR WK-END-OF-FILE
                       EXEC CICS
                            READNEXT
                            FILE('EDP3CR')
                            RIDFLD(WK-LLAVE-AUX)
                            INTO(REG-EDMACR)
                            NOHANDLE
                       END-EXEC
                       EVALUATE EIBRESP
                           WHEN DFHRESP(ENDFILE)
                               SET WK-END-OF-FILE TO TRUE
                               PERFORM 070-REGRESAR-TRAN
                           WHEN DFHRESP(DUPKEY)
                           WHEN DFHRESP(NORMAL)
                               CONTINUE
                           WHEN OTHER
                               MOVE 'OCURRIO UN ERROR' TO EDCLQ-MENSAJEO
                       END-EVALUATE
                   END-PERFORM
                   EXEC CICS
                        READNEXT
                        FILE('EDP3CR')
                        RIDFLD(WK-LLAVE-AUX)
                        INTO(REG-EDMACR)
                        NOHANDLE
                   END-EXEC
                   EVALUATE EIBRESP
                       WHEN DFHRESP(ENDFILE)
                           SET WK-END-OF-FILE TO TRUE
                       WHEN DFHRESP(DUPKEY)
                       WHEN DFHRESP(NORMAL)
                           PERFORM 220-MOVE-ADELANTE
                       WHEN OTHER
                           MOVE 'OCURRIO UN ERROR' TO EDCLQ-MENSAJEO
                   END-EVALUATE
               WHEN OTHER
                   MOVE 'OCURRIO UN ERROR START' TO EDCLQ-MENSAJEO
           END-EVALUATE.
      
      *---------- MOVER VALOR A CAMPOS ----------------*
       220-MOVE-ADELANTE.
           MOVE 1 TO WK-I.
           PERFORM UNTIL WK-END-OF-FILE OR WK-END-OF-I
               MOVE EDMP-LLAVE TO EDCLQ-COD-CREDITOO(WK-I)
               MOVE EDMP-CODIGO-CLIENTE TO EDCLQ-COD-CLIENTEO(WK-I)
               MOVE EDMP-MONTO-TOTAL TO EDCLQ-MONTO-TOTALO(WK-I)
               MOVE EDMP-SALDO-TOTAL TO EDCLQ-SALDO-TOTALO(WK-I)
               MOVE EDMP-CUOTA-MENSUAL TO EDCLQ-CUOTAO(WK-I)
               MOVE EDMP-FECHA-INICIO-PRESTAMO
               TO EDCLQ-FECHA-INICIOO(WK-I)
               IF EDMP-MARCA-ELIMINADO = 'D'
                   MOVE 'ELIMINADO' TO EDCLQ-ELIO(WK-I)
               ELSE
                   MOVE SPACE TO EDCLQ-ELIO(WK-I)
               END-IF
               MOVE DFHBMUNP TO EDCLQ-OPA(WK-I)
               MOVE SPACE TO EDCLQ-OPO(WK-I)
               ADD 1 TO WK-I
               EXEC CICS
                    READNEXT
                    FILE('EDP3CR')
                    RIDFLD(WK-LLAVE-AUX)
                    INTO(REG-EDMACR)
                    NOHANDLE
               END-EXEC
               IF EIBRESP = DFHRESP(ENDFILE)
                   SET WK-END-OF-FILE TO TRUE
               END-IF
           END-PERFORM.
           IF NOT WK-END-OF-FILE
               CONTINUE
               MOVE '+' TO  EDCLQ-SIGNOO
           ELSE
               MOVE ' ' TO EDCLQ-SIGNOO
               PERFORM UNTIL WK-END-OF-I
                   MOVE SPACE TO  EDCLQ-COD-CREDITOO(WK-I)
                   MOVE SPACE TO  EDCLQ-COD-CLIENTEO(WK-I)
                   MOVE SPACE TO  EDCLQ-MONTO-TOTALI(WK-I)
                   MOVE SPACE TO  EDCLQ-SALDO-TOTALI(WK-I)
                   MOVE SPACE TO  EDCLQ-CUOTAI(WK-I)
                   MOVE SPACE TO  EDCLQ-FECHA-INICIOI(WK-I)
                   MOVE SPACE TO  EDCLQ-ELIO(WK-I)
                   MOVE DFHBMPRO TO EDCLQ-OPA(WK-I)
                   MOVE SPACE TO EDCLQ-OPO(WK-I)
                   ADD 1 TO WK-I
               END-PERFORM
           END-IF.
           EXEC CICS
                ENDBR
                FILE ('EDP3CR')
           END-EXEC.
      
      *------------ GUARDAR LLAVES
       250-GUARDAR-LLAVES.
           MOVE EDCLQ-COD-CLIENTEO(13) TO
           WK-ULT-POS(WK-PANTALLA-ACTUAL)
           MOVE EDCLQ-COD-CREDITOO(13) TO
               WK-ULT-POS-CRE(WK-PANTALLA-ACTUAL)
           MOVE EDCLQ-COD-CREDITOO(1) TO
               WK-PRI-POS-CRE(WK-PANTALLA-ACTUAL)
           MOVE EDCLQ-COD-CLIENTEO(1) TO
           WK-PRI-POS(WK-PANTALLA-ACTUAL).
      
      *----------------- LEER HACIA ATRAS -----------------*
       300-CARGAR-PANTALLA.
           MOVE WK-PRI-POS(WK-PANTALLA-ACTUAL) TO WK-LLAVE-AUX
           EXEC CICS
                STARTBR
                FILE('EDP3CR')
                RIDFLD(WK-LLAVE-AUX)
                GTEQ
                NOHANDLE
           END-EXEC.
           EVALUATE EIBRESP
               WHEN DFHRESP(NOTOPEN)
                   MOVE 'ARCHIVO NO ABIERTO' TO EDCLQ-MENSAJEO
               WHEN DFHRESP(NORMAL)
                   PERFORM UNTIL EDMP-LLAVE =
                   WK-PRI-POS-CRE(WK-PANTALLA-ACTUAL) OR WK-END-OF-FILE
                       EXEC CICS
                            READNEXT
                            FILE('EDP3CR')
                            RIDFLD(WK-LLAVE-AUX)
                            INTO(REG-EDMACR)
                            NOHANDLE
                       END-EXEC
                       EVALUATE EIBRESP
                           WHEN DFHRESP(ENDFILE)
                               SET WK-END-OF-FILE TO TRUE
                               PERFORM 070-REGRESAR-TRAN
                           WHEN DFHRESP(DUPKEY)
                           WHEN DFHRESP(NORMAL)
                               CONTINUE
                           WHEN OTHER
                               MOVE 'OCURRIO UN ERROR' TO EDCLQ-MENSAJEO
                       END-EVALUATE
                   END-PERFORM
                   PERFORM 220-MOVE-ADELANTE.
      
      *----------- BUSCAR CLIENTE EN MAESTRO CLIENTE ------------*
       400-BUSCAR-CLIENTE.
           MOVE 1 TO WK-J.
           PERFORM VARYING WK-I FROM 1 BY 1 UNTIL WK-I > 13
               IF EDCLQ-OPI(WK-I) GREATER SPACE
                   MOVE WK-PANTALLA-ACTUAL  TO WK-PANTALLA-ACTUAL-EN
                   MOVE EDCLQ-COD-CLIENTEI(WK-I) TO WK-SEL-ENVIAR(WK-J)
                   MOVE EDCLQ-SIGNO2I TO WK-SIGNO-EN
                   ADD 1 TO WK-J
               END-IF
           END-PERFORM.
           IF WK-SEL-ENVIAR(1) > SPACE
               EXEC CICS
                    XCTL
                    PROGRAM('EDGD1CL3')
                    COMMAREA(WK-COM-ENVIAR)
               END-EXEC
           ELSE
                MOVE 'Deebe seleccionar algun registro en OP'
                TO EDCLQ-MENSAJEO
           END-IF.
           PERFORM 060-ENVIAR-DATOS.
           PERFORM 070-REGRESAR-TRAN .
      
      *------------ BUSCAR CURSOR EN PANTALLA
       500-BUSCAR-CURSOR.
           MOVE EIBCPOSN TO WK-POS-CURSOR.
           COMPUTE WK-FILA-CURSOR = (WK-POS-CURSOR / 132) + 1
           SUBTRACT 6 FROM WK-FILA-CURSOR GIVING WK-REGISTRO
           IF WK-REGISTRO < 1 OR WK-REGISTRO > 13
               MOVE 'Coloque cursor sobre un registro valido'
               TO EDCLQ-MENSAJEO
           ELSE
               IF EDCLQ-COD-CLIENTEI(WK-REGISTRO) = SPACE
                   MOVE 'Coloque cursor sobre un registro valido'
                   TO EDCLQ-MENSAJEO
               ELSE
                   MOVE WK-PANTALLA-ACTUAL  TO WK-PANTALLA-ACTUAL-EN
                   MOVE EDCLQ-COD-CLIENTEI(WK-REGISTRO) TO
                   WK-SEL-ENVIAR(1)
                   MOVE EDCLQ-SIGNO2I TO WK-SIGNO-EN
                   EXEC CICS
                        XCTL
                        PROGRAM('EDGD1CL3')
                        COMMAREA(WK-COM-ENVIAR)
                   END-EXEC
               END-IF
           END-IF.
      
       999-SALIR.
           EXEC CICS
                XCTL PROGRAM('EDGD1YL3')
           END-EXEC.
           GOBACK.