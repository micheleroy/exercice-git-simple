/**************************************************************************************** DEAL INFORMATIQUE **!
!&DOC-TECHNIQUE ! Deal/bacchus/cmm/bt-ediplug.p                                                               !
!===============!=============================================================================================!
!&Fiche       Du   ! S.!   Operateur   !                      Motif de l'intervention                         !
!__________________!___!_______________!_____________________________________________________________________-!
!V61 60300 19-07-16!Evo!mic            !faire une version 6 a partir de la 5 de tx2 pour Cocumont qui gere le !
!V61 59398 19-05-16!New!mic            !ajout prm pour editer la liste des commandes avec desadv et/pou facedi!
!V61 59235 03-05-16!New!mic            !remonter modif de fabien du edi-plug.p de 502 pour soc 0007 qui part s!
!V61 58589 17-03-16!New!mic            !mauvais test sur infoean                                              !
!V61 58544 15-03-16!New!mic            !ajout procedure controle des cdes en cours non livrées avec aviexp et/!
!                  !   !               !que les produits ont des ean btle et des ean carton                   !
!V61 57857 02-02-16!New!mic            !pour 238 - en maj copier le cdealg.Asc en cdealg.operat pour qu'il ne !
!V61 51392 17-12-14!New!mic            !integration cde edi prodiffu                                          !
!V61 47007 27-05-14!Evo!mic            !pb pour remonter commentaire de comment.Asc                           !
!V61 44845 19-02-14!New!mic            !edition auto avec ediplu                                              !
!V61 44609 10-02-14!Evo!mic            !edition auto avec motcle EDIPLU                                       !
!V61 44112 22-01-14!Evo!mic            !spool boncnt des alg                                                  !
!_____________________________________________________________________________________________________________!
!                             I N C L U D E S                !                    T A B L E S                 !
!____________________________________________________________+________________________________________________!
!d-brokmini.i          !NEW                                  !tabbac                                          !
!bt-edi-plus.i         !"new"                                !parstk                                          !
!bt-socdec.i           !                                     !cmdent                                          !
!bt-socini.i           !                                     !typcom                                          !
!maquette.i            !"new"                                !auxbac                                          !
!emilie/dog-def.i      !NEW GLOBAL                           !cmdlig                                          !
!regs-cha.i            !                                     !artbac                                          !
!out-logimp.i          !new                                  !condit                                          !
!bat-ini.i             !"new"                                !impbac                                          !
!bat-cha.i             !"periph" "periph" "a"                !                                                !
!maq-out.i             !"maquette" "ma-maquet" "periph" "0"  !                                                !
!maq-maj1.i            !"02" "edi"                           !                                                !
!maq-edi.i             !"02"                                 !                                                !
!regs-rec.i            !                                     !                                                !
!_____________________________________________________________________________________________________________!
!                                                        M A I N                                              !
!_____________________________________________________________________________________________________________!
&End
**************************************************************************************************************/
{ d-brokmini.i NEW }

{ bt-edi-plus.i "new" }

{ bt-socdec.i }
{ bt-socini.i }
{ maquette.i  "new" }



{ emilie/dog-def.i NEW GLOBAL}

regs-app = "elodie".
{regs-cha.i}


{ out-logimp.i new } /*- Gestion recap des spools pour Dog -*/

def input parameter  fichier      as char format "x(12)" .

def var periph                    as char no-undo.
def var zedi-maj                  as char no-undo.

DEF VAR sov-ident                 AS CHAR NO-UNDO.
DEF VAR sov-codsoc                AS CHAR NO-UNDO.

DEF VAR hdlwidget                 AS HANDLE.

RUN emilie/dog-function.p PERSISTENT SET HdlWidget .
SESSION:ADD-SUPER-PROCEDURE ( HdlWidget ) .

OUTPUT STREAM reponse TO "bt-ediplug-reponse.log".


/*----------------------------------------------------------*/
{bat-ini.i "new" }
run bat-lec.p (fichier) .


{bat-cha.i "periph"       "periph"             "a" }
{bat-cha.i "choix-edi"    "zedi-maj"           "a" }


{bat-cha.i "charg1"        "zone-charg[1]"     "a" }   
{bat-cha.i "charg2"        "zone-charg[2]"     "a" }   
{bat-cha.i "charg3"        "zone-charg[3]"     "a" }   
{bat-cha.i "charg4"        "zone-charg[4]"     "a" }   
{bat-cha.i "charg5"        "zone-charg[5]"     "a" }   
{bat-cha.i "charg6"        "zone-charg[6]"     "a" }   
{bat-cha.i "charg7"        "zone-charg[7]"     "a" }            
{bat-cha.i "charg8"        "zone-charg[8]"     "a" }    
{bat-cha.i "charg9"        "zone-charg[9]"     "a" }       
{bat-cha.i "ident"         "glo-ident"         "a" }              
{bat-cha.i "codsoc"        "glo-codsoc"        "a" }             
{bat-cha.i "operat"        "glo-operat"        "a" }         


ASSIGN codsoc-soc      = glo-codsoc
       nom-appl-client = glo-ident
       operat          = glo-operat
       broker-trav     = ""
       edi-maj         = int( zedi-maj ).



output stream gg to "bt-ediplug.log".

dog-ident  = glo-ident.
dog-operat = glo-operat.
operat     = glo-operat.
codsoc-soc = glo-codsoc.

put stream gg unformatted " avt edi-plug broker-trav " broker-trav skip.
put stream gg unformatted " edi-maj " + string( edi-maj ) skip.
put stream gg unformatted " periph " + periph skip.

put stream gg unformatted " glo-operat " + glo-operat skip.
put stream gg unformatted " dog-operat " + dog-operat skip.
put stream gg unformatted " operat " + operat skip.


put stream gg unformatted " glo-ident " + glo-ident skip.
put stream gg unformatted " dog-ident " + dog-ident skip.

Assign ma-entete = "bt-ediplu-e.p"
       ma-pied   = "bt-ediplu-p.p" .
       ma-maquet = "ediplu.maq".


run maq-lect.p .                                               

if   ma-anom <> ""
then do:

     message "Erreur sur ouverture de la maquette " ma-maquet 
              "anomalie " ma-anom view-as alert-box.
     return .

end .


{maq-out.i "maquette" "ma-maquet" "periph" "0" }   


                                                /* Nom du fichier cree par EDIPLUS */
def var version             as char format "x".
def var fichier-lot         as char format "x(55)" initial  "cdealg.asc".
def var fichier-quo         as char format "x(55)" initial  "cdealg.quo".
DEF VAR comlig-lot          AS CHAR FORMAT "x(55)" INITIAL  "coment.asc".
DEF VAR comlig-quo          AS CHAR FORMAT "x(55)" INITIAL  "coment.quo".

DEF VAR v-max               AS INT  NO-UNDO.

def var fichier-ctrl        as char format "x(30)" .
def var directory-lot       as char format "x(40)" .
Def var a                   as int .
Def var nom-complet         as char format "x(200)" .
Def var nom-complet1        as char format "x(200)" .
Def var nom-complet2        as char format "x(200)" .
Def var commande            as char format "x(50)" .
DEF VAR ztoday              AS CHAR FORMAT "x(6)".
DEF VAR zheure              AS CHAR FORMAT "x(5)".


DEF NEW SHARED STREAM      lecture.
def new shared stream      fic-lot .
def new shared stream      ficlog .

Def var imp-impauto         like impbac.imp-auto.

PUT STREAM gg UNFORMATTED " glo-ident " + glo-ident SKIP.
PUT STREAM gg UNFORMATTED " glo-operat " + glo-operat SKIP.


if   glo-ident = "401"   
then assign                                                                     /* si bouey */
     fichier-lot = "CMD93A.txt"
     fichier-quo = "statcmdo.quo" .

/*------------------------------------------------------------------*/
/* Recuperation de la directory de reception parametree dans TABLES */
/*------------------------------------------------------------------*/

IF   glo-ident = "238" 
THEN do:

     find tabbac where tabbac.codsoc = ""
                 and   tabbac.etabli = "LOT"
                 and   tabbac.typtab = "DIR"
                 and   tabbac.prefix = "GENCOD"
                 and   tabbac.codtab = "REC-EDIPLU"
                 no-lock no-error.

END.
ELSE DO:



     FIND tabbac WHERE tabbac.codsoc = glo-ident
                 and   tabbac.etabli = ""
                 and   tabbac.typtab = "PRM"
                 and   tabbac.prefix = "CDEEDI"
                 and   tabbac.codtab = codsoc-soc
                 NO-LOCK NO-ERROR.


     IF   NOT AVAILABLE tabbac 
     THEN FIND tabbac WHERE tabbac.codsoc = glo-ident
                      and   tabbac.etabli = ""
                      and   tabbac.typtab = "PRM"
                      and   tabbac.prefix = "CDEEDI"
                      and   tabbac.codtab = ""
                      NO-LOCK NO-ERROR.


     IF   NOT AVAILABLE tabbac 
     THEN find tabbac where tabbac.codsoc = codsoc-soc
                      and   tabbac.etabli = "LOT"
                      and   tabbac.typtab = "DIR"
                      and   tabbac.prefix = "GENCOD"
                      and   tabbac.codtab = "REC"
                      no-lock no-error.

     IF   NOT AVAILABLE tabbac                                                /* Nom directorie reception */
     THEN find tabbac where tabbac.codsoc = ""
                      and   tabbac.etabli = "LOT"
                      and   tabbac.typtab = "DIR"
                      and   tabbac.prefix = "GENCOD"
                      and   tabbac.codtab = "REC"
                      no-lock no-error.

     PUT STREAM gg UNFORMATTED " avai 4 tabbac" + STRING ( AVAILABLE tabbac ) SKIP.
END.

PUT STREAM gg UNFORMATTED "  tabbac" + tabbac.codsoc + " /" + tabbac.etabli + "/" + tabbac.typtab + "/" + tabbac.prefix + "/" + tabbac.codtab SKIP.
/* Nom directorie reception */
if   available tabbac               /* en principe dka0:trav.allegro.recept */
then assign directory-lot = tabbac.libel1[1]
            nom-complet2  = tabbac.libel1[2] 
            .
else do :

     message "PARAMETRAGE CHEMIN DE RECEPTION NON EFFECTUE ..." 
            VIEW-AS ALERT-BOX.
     RETURN "".
end .


Find tabbac where tabbac.codsoc = " "
            and   tabbac.etabli = " "
            and   tabbac.typtab = "ALG"
            and   tabbac.prefix = "VERSION"
            and   tabbac.codtab = " "
            no-lock no-error.

if   available tabbac 
then version = string (tabbac.nombre[1]).
else do:

     Find tabbac where tabbac.codsoc = glo-ident
                 and   tabbac.etabli = " "
                 and   tabbac.typtab = "PRM"
                 and   tabbac.prefix = "ALG"
                 and   tabbac.codtab = "VERSION"
                 no-lock no-error.

     IF AVAILABLE tabbac     THEN version = STRING( tabbac.libel1[1]).

     IF NOT AVAILABLE tabbac THEN version = "1".

END.


IF  glo-ident = "502" 
AND glo-codsoc = "0007" THEN VERSION = "4".


ASSIGN  nom-complet  = trim ( directory-lot) + trim ( fichier-lot )
        nom-complet1 = "".



if   search ( nom-complet ) = ?
then nom-complet = trim( directory-lot ) + trim( caps( fichier-lot )).

PUT STREAM gg UNFORMATTED "nom-copmlet" + nom-complet SKIP.

if   search ( nom-complet ) = ?
then do:

     PUT STREAM gg UNFORMATTED " pas de fichier d'integration" SKIP.

     message "Pas de fichier d'integration"
             view-as alert-box.
     return.

end.


put stream gg unformatted "traitement fichier " nom-complet skip.

     FIND tabbac WHERE tabbac.codsoc = glo-ident
                 and   tabbac.etabli = ""
                 and   tabbac.typtab = "PRM"
                 and   tabbac.prefix = "CDEEDI"
                 and   tabbac.codtab = "COPYAVT"
                 NO-LOCK NO-ERROR.

     IF  ( AVAILABLE tabbac 
     OR   glo-ident = "238" )
     AND  int( zedi-maj) <> 0

     THEN DO:


          assign  ztoday = STRING( YEAR( TODAY), "9999")
                  ztoday = SUBSTRING( ztoday, 3, 2)
                         + STRING( MONTH( TODAY), "99")
                         + STRING( DAY ( TODAY) , "99")
                  zheure = STRING( TIME, "HH:MM")
                  zheure = SUBSTRING( zheure, 1, 2) + SUBSTRING( zheure, 4, 2)
                  zheure = TRIM( zheure ).


          nom-complet1 = nom-complet.

   
          nom-complet  =  trim ( directory-lot) + "cdealg" + ztoday + zheure + "." +  glo-operat .

          
         
          OS-COPY   VALUE ( nom-complet1 ) VALUE ( nom-complet ).
          PUT STREAM gg  UNFORMATTED " os-copy nom-complet1 " + nom-complet1  + " to nom-complet " + nom-complet SKIP.

          OS-DELETE VALUE ( nom-complet1 ) . 

          PUT STREAM gg UNFORMATTED " os-delete nom-complet1" SKIP.

          PUT STREAM gg UNFORMATTED " maintenant on travaille avec nom-complet " nom-complet SKIP.

          /* je copie le fichier pour qu'il ne soit pas prie par d'autres */

     END.



put stream gg unformatted "glo-ident " + glo-ident skip.
PUT STREAM gg UNFORMATTED " version " + VERSION SKIP.
PUT STREAM gg UNFORMATTED " artbac-oc " +  artbac-soc + "stocks-soc " + stocks-soc + "glo-codsoc " + glo-codsoc SKIP.

/*-----------------------------------------------*/
/* Chercher le code de l'emballage = carton de 1 */
/*-----------------------------------------------*/

Find Parstk where parstk.codsoc = parstk-soc
            no-lock no-error.

if   available Parstk
then cdl-echantillon = parstk.articl-emb.
else cdl-echantillon = "".

/*-------------------*/
/* Valeur du tonneau */
/*-------------------*/

Find Tabbac where Tabbac.codsoc = tabbac-soc
            and   tabbac.etabli = ""
            and   tabbac.typtab = "CON"
            and   tabbac.prefix = "TONNEAU"
            and   tabbac.codtab = ""
            no-lock no-error.

if   available tabbac
then cde-tonneau = tabbac.nombre[1].
else cde-tonneau = 1.

z-ficlot-ok = 0.



PUT STREAM gg UNFORMATTED " avt hdl-progdog se" SKIP.


PUT STREAM gg UNFORMATTED " search ( bacchus/dog-cmdent.p ) " + search ( "bacchus/dog-cmdent.p" ) SKIP.


PUT STREAM gg UNFORMATTED " apres hdl-progdog" SKIP.


noligtxt = 0.
input stream lecture from value ( nom-complet ) no-echo .

PUT STREAM gg UNFORMATTED " apres import sream " SKIP.

IF  ( glo-ident = "238" )
THEN DO:



     repeat :

            import stream lecture UNFORMATTED lig-lue .


            PUT STREAM gg UNFORMATTED " lecture lig-lue "  + lig-lue SKIP.

            assign
            z-ean-cde         =       substring ( lig-lue , 1  , 13)
            z-refcli          =       substring ( lig-lue , 40 , 10 ).


            if ( z-ean-cde + z-refcli <> z-cdeprec )  
            OR   z-cdeprec = ""
            then DO:


                 noligtxt = noligtxt + 1 .

                 edi = "".
                 edi [1] = "Cde a Intégrer : refcde " + z-refcli + "EAN cde " + z-ean-cde.


                 {maq-maj1.i "02" "edi" }
                 {maq-edi.i "02" }

            END.
            z-cdeprec = z-ean-cde + z-refcli.


     end.


     edi [1] = "" . 

     {maq-maj1.i "02" "edi" }
     {maq-edi.i "02" }

     edi [1] = string( noligtxt ) + " Commandes a Intégrer " .


     {maq-maj1.i "02" "edi" }
     {maq-edi.i "02" }

     edi [1] = "" . 

     {maq-maj1.i "02" "edi" }
     {maq-edi.i "02" }

end.

INPUT STREAM lecture CLOSE.


input stream fic-lot from value ( nom-complet ) no-echo .

ASSIGN z-ean-cde  = ""
       z-refcli   = ""
       z-cdeprec  = ""
       sov-ident  = glo-ident
       sov-codsoc = glo-codsoc
       noligtxt   = 1.
    .


repeat :

    glo-ident  = sov-ident.
    glo-codsoc = sov-codsoc.

    import stream fic-lot UNFORMATTED lig-lue .

    PUT STREAM gg UNFORMATTED "glo-ident " + glo-ident SKIP.
    PUT STREAM gg UNFORMATTED "bt-edi-plug " + lig-lue SKIP.




    if   glo-ident = "401" 
    then run bt-edi-axon.p  .               /* BOUEY */
    else do:

         CASE version :

              WHEN "0" THEN RUN bt-edi-plu0.p.  /* version INFLUE- Producta */
              WHEN "1" THEN RUN bt-edi-plu1.p.  /* version INFLUE  */
              WHEN "2" THEN RUN bt-edi-plu6.p.
              WHEN "3" THEN RUN bt-edi-plu11.p. /* version gexedi - */
              WHEN "4" THEN RUN bt-edi-plu12.p. /* version @gp  */ 
              WHEN "5" THEN RUN bt-edi-plu13.p. /* version TX2 - Prodiffu  */ 
              WHEN "6" THEN RUN bt-edi-plu14.p. /* version TX2 - Cocumont  */ 

              OTHERWISE DO:

                        message "La version d' ALLEGRO n'est pas parametree !" 
                                view-as alert-box.
                        RETURN "".

              END.

         END CASE.
    end.
end.

if   int( zedi-maj) <> 0
then run bt-edi-plu4.p .          /* module fin traitement de la
                                        derniere commande du fichier */

      edi = "".
     {maq-maj1.i "02" "edi" }
     {maq-edi.i "02" }


IF   edi-maj = 1 
THEN DO:

     assign  ztoday = STRING( YEAR( TODAY), "9999")
             ztoday = SUBSTRING( ztoday, 3, 2)
                    + STRING( MONTH( TODAY), "99")
                    + STRING( DAY ( TODAY) , "99")
             zheure = STRING( TIME, "HH:MM")
             zheure = SUBSTRING( zheure, 1, 2) + SUBSTRING( zheure, 4, 2)
             zheure = TRIM( zheure ).

     if   nom-complet2 = ""
     then assign  a            = length ( nom-complet )
                  nom-complet2 = trim(substring(nom-complet ,1 ,a - 4))
                  nom-complet2 = nom-complet2 + ztoday + zheure + ".int" .

     else nom-complet2 = nom-complet2 + "cdealg" + ztoday + zheure + ".int".


     edi [1] = "Fichier Sauvegarder en " + nom-complet2 .


     {maq-maj1.i "02" "edi" }
     {maq-edi.i "02" }





     OS-COPY   VALUE ( nom-complet ) VALUE ( nom-complet2 ).
     OS-DELETE VALUE ( nom-complet ) . . 


     edi [1] = "" .


    {maq-maj1.i "02" "edi" }
    {maq-edi.i "02" }


    edi [1] = "Verification des Anomalies sur Commandes En cours Avec AviEXP et/ou FacEDI " .


    {maq-maj1.i "02" "edi" }
    {maq-edi.i "02" }


     FIND tabbac WHERE tabbac.codsoc = glo-ident
                 AND   tabbac.etabli = ""
                 AND   tabbac.typtab = "PRM"
                 AND   tabbac.prefix = "EDIPLU"
                 AND   tabbac.codtab = "CNTEAN"
                 NO-LOCK NO-ERROR.

     IF   AVAILABLE tabbac 
     THEN DO:


         FOR EACH cmdent WHERE cmdent.codsoc     = glo-codsoc
                         AND   cmdent.a-facturer = ""
                         AND   ( cmdent.avis-edi = YES
                         OR      cmdent.fact-edi = YES )
                         NO-LOCK :

                FIND typcom WHERE typcom.codsoc = typcom-soc
                            AND   typcom.typcom = cmdent.typcom
                            NO-LOCK NO-ERROR.

                IF NOT AVAILABLE typcom THEN NEXT.

                IF   LOOKUP( "ALLEGR" , typcom.edition ) = 0  
                THEN NEXT.

                regs-fileacc = "AUXILI" + cmdent.typaux .
                {regs-rec.i}
                codsoc-aux = regs-soc .

                find auxbac where auxbac.codsoc = codsoc-aux
                            and   auxbac.typaux = cmdent.typaux
                            AND   auxbac.codaux = cmdent.codaux
                            no-lock no-error .
                IF   NOT AVAILABLE auxbac 
                OR   auxbac.gencod = ""
                THEN DO:

                     edi[1] = "Cde " + cmdent.typcom + "." + cmdent.commande + "." + cmdent.nivcom + "Tiers Cde " + cmdent.typaux + cmdent.codaux + " Sans Gencod" .

                    {  maq-maj1.i "05" "edi" }
                    {  maq-edi.i "05" }



                END.

                regs-fileacc = "AUXILI" + cmdent.typaux-liv .
                {regs-rec.i}
                codsoc-aux = regs-soc .

                find auxbac where auxbac.codsoc = codsoc-aux
                            and   auxbac.typaux = cmdent.typaux-liv
                            AND   auxbac.codaux = cmdent.codaux-liv
                            no-lock no-error .

                IF   NOT AVAILABLE auxbac 
                OR   auxbac.gencod = ""
                THEN DO:

                    edi[1] = "Cde " + cmdent.typcom + "." + cmdent.commande + "." + cmdent.nivcom + "Tiers Liv " + cmdent.typaux-liv + cmdent.codaux-liv + " Sans Gencod" .

                   {  maq-maj1.i "05" "edi" }
                   {  maq-edi.i "05" }



                END.

                regs-fileacc = "AUXILI" + cmdent.typaux-fac .
                {regs-rec.i}
                codsoc-aux = regs-soc .

                find auxbac where auxbac.codsoc = codsoc-aux
                            and   auxbac.typaux = cmdent.typaux-fac
                            AND   auxbac.codaux = cmdent.codaux-fac
                            no-lock no-error .

                IF  ( NOT AVAILABLE auxbac 
                OR   auxbac.gencod = "" )
                AND  cmdent.fact-edi 
                THEN DO:

                    edi[1] = "Cde " + cmdent.typcom + "." + cmdent.commande + "." + cmdent.nivcom + "Tiers fac" + cmdent.typaux-fac + cmdent.codaux-fac + " Sans Gencod" .

                   {  maq-maj1.i "05" "edi" }
                   {  maq-edi.i "05" }


                END.

                FOR EACH cmdlig no-lock
                                WHERE  cmdlig.codsoc   = cmdent.codsoc
                                AND    cmdlig.etabli   = cmdent.etabli
                                AND    cmdlig.typiec   = cmdent.typiec
                                AND    cmdlig.typcom   = cmdent.typcom
                                AND    cmdlig.commande = cmdent.commande
                                AND    cmdlig.nivcom   = cmdent.nivcom
                                AND    cmdlig.articl  <> ""
                                , EACH artbac NO-LOCK
                                WHERE  artbac.codsoc   = artbac-soc
                                AND    artbac.articl   = cmdlig.articl
                                :


                            IF   LENGTH ( artbac.pays-gencod + artbac.cnuf + artbac.gencod ) <> 13 

                            THEN DO:


                                 IF   LENGTH ( artbac.info-ean ) <> 12 
                                 THEN DO:


                                 edi[1] = "Cde " + cmdent.typcom + "." + cmdent.commande + "." + cmdent.nivcom + "Produit" + cmdlig.articl + " Gencod Incomplet" .

                                 {  maq-maj1.i "05" "edi" }
                                 {  maq-edi.i "05" }

                                 END.

                            END.

                            FIND condit WHERE condit.codsoc     = artbac-soc
                                        AND   condit.articl     = cmdlig.articl
                                        AND   condit.articl-emb = cmdlig.articl-emb
                                        AND   condit.date-tarif = ?
                                        NO-LOCK NO-ERROR.

                            IF   AVAILABLE condit 
                            AND  length( condit.pays-gencod-emb + condit.cnuf-emb + condit.gencod-emb ) <> 13
                            THEN DO:

                                 edi[1] = "Cde " + cmdent.typcom + "." + cmdent.commande + "." + cmdent.nivcom + "Produit" + cmdlig.articl + " Emb " + cmdlig.articl-emb + " Gencod Emballage Incomplet" .

                                 {  maq-maj1.i "05" "edi" }
                                 {  maq-edi.i "05" }


                            END.


                            Find Condit where Condit.codsoc     = artbac-soc
                                        and   Condit.articl     = cmdlig.articl
                                        and   Condit.articl-emb = cmdlig.articl-emb
                                        and   Condit.articl-pal <> ""
                                        and   Condit.date-tarif = ?
                                        no-lock no-error.

                            if   available Condit 
                            AND  length( condit.pays-gencod-pal + condit.cnuf-pal + condit.gencod-pal ) <> 13
                            THEN DO:

                                 edi[1] = "Cde " + cmdent.typcom + "." + cmdent.commande + "." + cmdent.nivcom + "Produit" + cmdlig.articl + " Emb " + cmdlig.articl-emb + " Gencod Palette Incomplet" .

                                 {  maq-maj1.i "05" "edi" }
                                 {  maq-edi.i "05" }


                            END.




                 END.






         END.

     END.

end.

/*--------------------------------------------------------*/
/* Acces aux fichiers COMent.ASC  dans la bonne directory */
/*--------------------------------------------------------*/

ASSIGN  comlig-lot  = "coment.asc"
        nom-complet = trim ( directory-lot) + trim ( comlig-lot ).

  PUT STREAM gg UNFORMATTED " avai comlig-lot "  SKIP.

  PUT STREAM gg UNFORMATTED " search comlig-lot " + SEARCH ( nom-complet ) SKIP.

if   search ( nom-complet ) <> ?
then do:

     ASSIGN  z-ficlot-ok   = 0
             nom-complet = trim( directory-lot ) + trim( comlig-lot ) 
             fichier-lot = directory-lot + comlig-lot .

     PUT STREAM gg UNFORMATTED "avant input "   SKIP.

     input stream fic-lot from value ( nom-complet ) no-echo .

     import stream fic-lot UNFORMATTED lig-lue .

     PUT STREAM gg UNFORMATTED " lig-lue " + lig-lue  SKIP.

     repeat while lig-lue <> "" :



         PUT STREAM gg UNFORMATTED " version " + STRING ( VERSION ) SKIP.

        CASE version :

             WHEN "4" THEN run bt-edi-plub2.p. /* version @gp pour Bestheim */ 

             OTHERWISE run bt-edi-plub.p .

        END CASE.

        import stream fic-lot UNFORMATTED lig-lue .

     end.

     ASSIGN  comlig-lot  = "coment.asc"
             comlig-lot  = trim ( directory-lot) + trim ( comlig-lot )

             a           = length ( trim( comlig-lot ) ) 
             nom-complet = trim(substring(comlig-lot,1,a - 4)) 
             nom-complet = nom-complet + ztoday + zheure + ".int".


     OS-COPY   VALUE ( comlig-lot ) VALUE ( nom-complet ).
     OS-DELETE VALUE ( comlig-lot ) . . 







end. /* searcjh nom-complet */


output stream maquette close .



           Find Impbac where Impbac.ope    = Operat
                       and   Impbac.motcle = "EDIPLU"
                       no-lock no-error.


           if   not available Impbac
           then do:

                Find Impbac where Impbac.ope    = ""
                            and   Impbac.motcle = "EDIPLU"
                            no-lock no-error.

           END.



           IF   AVAILABLE impbac 
           THEN DO:



                 put stream gg unformatted " avt bat-turbo " + impbac.imp-auto + "  du peroh " + periph skip.

                     RUN bat-turbo.p ( impbac.imp-auto , periph ).





           END.







put stream gg unformatted " apres edi-plug avec impbac EDIPLU" skip.














