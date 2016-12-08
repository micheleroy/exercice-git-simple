/**************************************************************************************** DEAL INFORMATIQUE **!
!&DOC-TECHNIQUE ! Deal/cmm/cmm/bt-blo-deblo-stopmp.p                                                          !
!===============!=============================================================================================!
!&Fiche       Du   ! S.!   Operateur   !                      Motif de l'intervention                         !
!__________________!___!_______________!_____________________________________________________________________-!

|          08-12-16!   !mic            !modifier ce fichier sur mon pc que j'ai recuperer par pull            !    
!V61 61967 08-12-16!Evo!mic            !Blocage Fiche STOPMP                                                  !
!                  !   !               !Copy en : WF : 139661                                                 !
!_____________________________________________________________________________________________________________!
!                             I N C L U D E S                !                    T A B L E S                 !
!____________________________________________________________+________________________________________________!
!d-brokmini.i          !                                     !tabbac                                          !
!bt-calpmp.i           !"new"                                !tables                                          !
!maquette.i            !"new"                                !tabsoc                                          !
!bt-ident.i            !                                     !artbac                                          !
!bt-socdec.i           !                                     !stopmp                                          !
!bt-socini.i           !                                     !                                                !
!regs-cha.i            !                                     !                                                !
!select.i              !"new"                                !                                                !
!selectpt.i            !                                     !                                                !
!bat-ini.i             !"new"                                !                                                !
!bat-cha.i             !"periph" "z-periph" "a"              !                                                !
!regs-rec.i            !                                     !                                                !
!artbac-t.i            !                                     !                                                !
!select-z.i            !"magasin" "stopmp.magasin"           !                                                !
!majmoucb.i            !b-stopmp                             !                                                !
!_____________________________________________________________________________________________________________!
!                                                        M A I N                                              !
!_____________________________________________________________________________________________________________!
&End
**************************************************************************************************************/

{ d-brokmini.i }

{ bt-calpmp.i "new" } 
{ maquette.i  "new" }

deal-setsocreg( "" ) .
deal-setsocreg( glo-codsoc ).

/*--------------------------*/
/* Societes de regroupement */
/*--------------------------*/
{ bt-ident.i  }
{ bt-socdec.i }
{ bt-socini.i }

    /*----------------------------*/
/* chargement societes compta */
/*----------------------------*/
regs-app = "elodie".
{regs-cha.i}


{ select.i "new"}
{ selectpt.i  }  /* procedures pour selections */


/*---------------------------------------------------*/
/* chargement des choix en fonction du fichier batch */
/*---------------------------------------------------*/

def input parameter  fichier      as char format "x(12)" .
def var ztoday                    as char no-undo.
def var zlog                      as char no-undo.
def var zheure                    as char no-undo.


DEF BUFFER b-stopmp               FOR stopmp.


{bat-ini.i "new" }
run bat-lec.p (fichier) .

/*---------------------------------------------------------*/
/* Prise des selections et chargement des bornes mini-maxi */
/*---------------------------------------------------------*/



{bat-cha.i "periph"        "z-periph"           "a" }
{bat-cha.i "var-periph"    "z-periph"           "a" }
{bat-cha.i "var-perdeb"    "z-perdeb"           "a" }
{bat-cha.i "var-perfin"    "z-perfin"           "a" }

{bat-cha.i "var-blo"       "z-blo"              "a" }

{bat-cha.i "var-phase0"    "z-phase0"           "a" }
{bat-cha.i "var-phase1"    "z-phase1"           "a" }
{bat-cha.i "var-phase2"    "z-phase2"           "a" }
{bat-cha.i "var-phase3"    "z-phase3"           "a" }
{bat-cha.i "sel-applic"    "selection-applic"   "a" }
{bat-cha.i "sel-filtre"    "selection-filtre"   "a" }
{bat-cha.i "sel-sequence"  "selection-sequence" "a" }





run select-c.p.


IF z-phase1 = "true"  THEN z-phase1 = "yes" .
IF z-phase2 = "true"  THEN z-phase2 = "yes" .
IF z-phase3 = "true"  THEN z-phase3 = "yes" .



IF z-phase1 = "false" THEN z-phase1 = "no" .
IF z-phase2 = "false" THEN z-phase2 = "no" .
IF z-phase3 = "false" THEN z-phase3 = "no" .


     assign  ztoday = STRING( YEAR( TODAY), "9999")
             ztoday = SUBSTRING( ztoday, 3, 2)
                    + STRING( MONTH( TODAY), "99")
                    + STRING( DAY ( TODAY) , "99")
             zheure = STRING( TIME, "HH:MM")
             zheure = SUBSTRING( zheure, 1, 2) + SUBSTRING( zheure, 4, 2)
             zheure = TRIM( zheure )
             zlog   = "BLO-DEBLO-STOPMP" + ztoday + zheure + ".log".


OUTPUT STREAM gg TO value( zlog) .






if length( trim( z-perdeb)) <> 6 then return.
if length( trim( z-perfin)) <> 6 then return.

if substring( z-perdeb , 1, 4 ) < "2010" then return.
if substring( z-perdeb , 1, 4 ) > "2100" then return.
if substring( z-perdeb , 5, 2 ) < "01"   then return.
if substring( z-perdeb , 5, 2 ) > "12"   then return.

if substring( z-perfin , 1, 4 ) < "2010" then return.
if substring( z-perfin , 1, 4 ) > "2100" then return.
if substring( z-perfin , 5, 2 ) < "01"   then return.
if substring( z-perfin , 5, 2 ) > "12"   then return.





        Find FIRST tabbac where tabbac.codsoc = deal-getsocreg ( "tabbac")
                          and   tabbac.etabli = " "
                          and   tabbac.typtab = "CON"
                          and   tabbac.prefix = "DEPO"
                          no-lock no-error.

        if   not available tabbac
        then depot-defaut = "000".
        else depot-defaut = tabbac.codtab.



        z-depot = "".

        FOR EACH tabbac WHERE  tabbac.codsoc   = tabbac-soc
                        AND    tabbac.etabli   = ""
                        AND    tabbac.typtab   = "ART"
                        AND    tabbac.prefix   = "MAGASIN"
                        AND    SUBSTRING( tabbac.libel1[5], 1, 1) = "O"     /* calpmp Oui */
                        :

            IF   TRIM ( tabbac.libel1[6] ) = "" 
            THEN DO:

                 IF   z-depot = "" 
                 THEN z-depot = z-depot + tabbac.codtab.
                 ELSE z-depot = z-depot + "," + tabbac.codtab.

            END.
            ELSE DO:

                 IF LOOKUP ( TRIM ( glo-codsoc ) , tabbac.libel1[6] ) = 0  THEN NEXT.


                 IF   z-depot = "" 
                 THEN z-depot = z-depot + tabbac.codtab.
                 ELSE z-depot = z-depot + "," + tabbac.codtab.

            END.


        END.

        z-depot = z-depot + ",".

        PUT STREAM gg UNFORMATTED "z-depot "        + z-depot SKIP.
        PUT STREAM gg UNFORMATTED "avec blocage  "  + z-blo SKIP.

        PUT STREAM gg UNFORMATTED "z-phase0" + STRING( z-phase0 )  SKIP.
        PUT STREAM gg UNFORMATTED "z-phase1" + STRING( z-phase1 )  SKIP.
        PUT STREAM gg UNFORMATTED "z-phase2" + STRING( z-phase2 )  SKIP.
        PUT STREAM gg UNFORMATTED "z-phase3" + STRING( z-phase3 )  SKIP.



sov-stocks = stocks-soc.

DEF BUFFER b-tabsoc      FOR tabsoc.

for each B-tables where  B-tables.codsoc = ""
                  and    B-tables.etabli = ""
                  and    B-tables.typtab = "SOC"
                  and    B-tables.prefix = "SOCIETE"
                    , EACH b-tabsoc 
                  WHERE  b-tabsoc.applic = "BACCHUS"
                  AND    b-tabsoc.codsoc = b-tables.codtab 
                  no-lock :

    Assign
    codsoc-soc = B-tables.codtab
    regs-app   = "BACCHUS".
    {regs-cha.i }

    regs-fileacc = "STOCKS".
    {regs-rec.i }

    if   regs-soc = sov-stocks
    then do:

         IF tou-codsoc <> "" THEN tou-codsoc = tou-codsoc + ",".

         tou-codsoc = tou-codsoc + codsoc-soc .
    END.

end. /* for each tables */


     PUT STREAM gg UNFORMATTED "tou-codsoc"  + tou-codsoc SKIP.


     FIND tabbac WHERE tabbac.codsoc = tabbac-soc
                 AND   tabbac.etabli = ""
                 AND   tabbac.typtab = "ART"
                 AND   tabbac.prefix = "TYPTYP"
                 AND   tabbac.codtab = "TIB"
                 NO-LOCK NO-ERROR.

     IF   NOT AVAILABLE tabbac 
     THEN DO:

            PUT STREAM gg UNFORMATTED "Le type Article Identique : TIB n'est pas paramétré ......" SKIP.

            MESSAGE "Le type Article Identique : TIB n'est pas paramétré ......"
                    VIEW-AS ALERT-BOX.
            RETURN.
     END.

     tib-typart = tabbac.libel1[1].

     PUT STREAM gg UNFORMATTED " tib-typart " tib-typart SKIP.


     FIND tabbac WHERE tabbac.codsoc = tabbac-soc
                 AND   tabbac.etabli = ""
                 AND   tabbac.typtab = "ART"
                 AND   tabbac.prefix = "TYPTYP"
                 AND   tabbac.codtab = "PF"
                 NO-LOCK NO-ERROR.

     IF   NOT AVAILABLE tabbac 
     THEN DO:

            PUT STREAM gg UNFORMATTED "Le type Article Identique : PF n'est pas paramétré ......" SKIP.


            RETURN.
     END.

     pf-typart = tabbac.libel1[1].

     PUT STREAM gg UNFORMATTED " pf-typart " pf-typart SKIP.


{bt-socini.i }





run select-v.p ( "articl", input-output art-min, input-output art-max).


IF   z-phase1 = "yes" 
THEN DO:


     FOR EACH artbac WHERE   artbac.codsoc    = artbac-soc
                     AND   ( artbac.chateau   = "9"
                     OR      artbac.typart    BEGINS "V" 
                     OR      artbac.typart    = "MP"      )
                     AND     artbac.tenusto   <> "1"
                     AND     artbac.articl    >= art-min
                     AND     artbac.articl    <= art-max
                     NO-LOCK   :


           IF glo-ident = "502" AND artbac.typart BEGINS "V" THEN  NEXT.

           {artbac-t.i }
           IF selection-ok <> "" THEN NEXT.



           do i-i = int(z-perdeb) to int(z-perfin) :

                 zper     = STRING( i-i , "999999").
                 zperiode = " " + substring( zper, 5, 2 ).
                 zannee   = SUBSTRING( zper , 1 , 4 ).

                 FOR EACH  Stopmp USE-INDEX cle-art2
                                  WHERE Stopmp.codsoc       =  Stocks-soc     
                                  AND   Stopmp.articl       =  artbac.articl  
                                  AND   Stopmp.Annee        =  zannee                 
                                  AND   Stopmp.periode      =  zperiode                
                                  NO-LOCK :

                      IF z-depot <> "" AND LOOKUP( stopmp.magasin , z-depot ) = 0 THEN NEXT.


                      {select-z.i "magasin"       "stopmp.magasin"       }

                      run select-t.p .                                
                      IF selection-ok <> "" THEN NEXT.


                      FIND b-stopmp WHERE ROWID( b-stopmp ) = ROWID ( stopmp)
                                    EXCLUSIVE-LOCK NO-ERROR.


                      IF   AVAILABLE b-stopmp 
                      THEN DO:

                           {majmoucb.i b-stopmp }

                           IF z-blo = "BLO" THEN b-stopmp.opecre = "BLO".
                                            ELSE b-stopmp.opecre = b-stopmp.opemaj.


                           PUT STREAM gg UNFORMATTED b-stopmp.articl + " " + artbac.typart + " " + b-stopmp.periode + " " + b-stopmp.magasin + " " + b-stopmp.opecre SKIP.

                           VALIDATE b-stopmp.
                           RELEASE b-stopmp.

                      END.

                 END.


           END.




     END.


END.




IF   z-phase2 = "yes" 
THEN DO:


     PUT STREAM gg UNFORMATTED "tib-typart"  + tib-typart SKIP.

     FOR EACH artbac WHERE   artbac.codsoc    = artbac-soc
                     AND     lookup( artbac.typart, tib-typart ) <> 0 
                     AND     artbac.tenusto   <> "1"
                     AND     artbac.articl    >= art-min
                     AND     artbac.articl    <= art-max
                     NO-LOCK   :


           {artbac-t.i }
           IF selection-ok <> "" THEN NEXT.



           do i-i = int(z-perdeb) to int(z-perfin) :

                 zper     = STRING( i-i , "999999").
                 zperiode = " " + substring( zper, 5, 2 ).
                 zannee   = SUBSTRING( zper , 1 , 4 ).

                 FOR EACH  Stopmp USE-INDEX cle-art2
                                  WHERE Stopmp.codsoc       =  Stocks-soc     
                                  AND   Stopmp.articl       =  artbac.articl  
                                  AND   Stopmp.Annee        =  zannee                 
                                  AND   Stopmp.periode      =  zperiode                
                                  NO-LOCK :

                      IF z-depot <> "" AND LOOKUP( stopmp.magasin , z-depot ) = 0 THEN NEXT.


                      {select-z.i "magasin"       "stopmp.magasin"       }

                      run select-t.p .                                
                      IF selection-ok <> "" THEN NEXT.


                      FIND b-stopmp WHERE ROWID( b-stopmp ) = ROWID ( stopmp)
                                    EXCLUSIVE-LOCK NO-ERROR.


                      IF   AVAILABLE b-stopmp 
                      THEN DO:

                           {majmoucb.i b-stopmp }

                           IF z-blo = "BLO" THEN b-stopmp.opecre = "BLO".
                                            ELSE b-stopmp.opecre = b-stopmp.opemaj.

                           PUT STREAM gg UNFORMATTED b-stopmp.articl + " " + artbac.typart + " " + b-stopmp.periode + " " + b-stopmp.magasin + " " + b-stopmp.opecre SKIP.


                           VALIDATE b-stopmp.
                           RELEASE b-stopmp.

                      END.

                 END.


           END.




     END.


END.


IF   z-phase3 = "yes" 
THEN DO:


     PUT STREAM gg UNFORMATTED "pf-typart"  + pf-typart SKIP.

     FOR EACH artbac WHERE   artbac.codsoc    = artbac-soc
                     AND     lookup( artbac.typart, pf-typart ) <> 0 
                     AND     artbac.tenusto   <> "1"
                     AND     artbac.articl    >= art-min
                     AND     artbac.articl    <= art-max
                     NO-LOCK   :


           {artbac-t.i }
           IF selection-ok <> "" THEN NEXT.



           do i-i = int(z-perdeb) to int(z-perfin) :

                 zper     = STRING( i-i , "999999").
                 zperiode = " " + substring( zper, 5, 2 ).
                 zannee   = SUBSTRING( zper , 1 , 4 ).

                 FOR EACH  Stopmp USE-INDEX cle-art2
                                  WHERE Stopmp.codsoc       =  Stocks-soc     
                                  AND   Stopmp.articl       =  artbac.articl  
                                  AND   Stopmp.Annee        =  zannee                 
                                  AND   Stopmp.periode      =  zperiode                
                                  NO-LOCK :

                      IF z-depot <> "" AND LOOKUP( stopmp.magasin , z-depot ) = 0 THEN NEXT.

                      {select-z.i "magasin"       "stopmp.magasin"       }

                      run select-t.p .                                
                      IF selection-ok <> "" THEN NEXT.


                      FIND b-stopmp WHERE ROWID( b-stopmp ) = ROWID ( stopmp)
                                    EXCLUSIVE-LOCK NO-ERROR.


                      IF   AVAILABLE b-stopmp 
                      THEN DO:

                           {majmoucb.i b-stopmp }

                           IF z-blo = "BLO" THEN b-stopmp.opecre = "BLO".
                                            ELSE b-stopmp.opecre = b-stopmp.opemaj.

                           PUT STREAM gg UNFORMATTED b-stopmp.articl + " " + artbac.typart + " " + b-stopmp.periode + " " + b-stopmp.magasin + " " + b-stopmp.opecre SKIP.

                           VALIDATE b-stopmp.
                           RELEASE b-stopmp.

                      END.

                 END.


           END.




     END.


END.






OUTPUT STREAM gg CLOSE.






















