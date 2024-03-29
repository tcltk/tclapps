# da.msg --
#
#	Danish-language messages for KHIM
#
# Copyright (c) 2005 by Kevin B. Kenny.  All rights reserved.
# Translation by Torsten Berg
#
# Refer to the file "license.terms" for the terms and conditions of
# use and redistribution of this file, and a DISCLAIMER OF ALL WARRANTEES.
#
# $Id: da.msg,v 1.1 2006/09/05 13:48:49 kennykb Exp $
# $Source: /cvsroot/tcllib/tklib/modules/khim/da.msg,v $
#
#----------------------------------------------------------------------

namespace eval ::khim {

    ::msgcat::mcset da {Apply} {Anvend}

    ::msgcat::mcset da {Cancel} {Annuller}

    ::msgcat::mcset da {Change} {�ndre}

    ::msgcat::mcset da {Character} {Tegn}

    ::msgcat::mcset da {Compose Key} {Compose taste}

    ::msgcat::mcset da {Compose key:} {Compose taste:}

    ::msgcat::mcset da {Composed sequence must be two characters long} \
	{Compose tegnf�lgen skal best� af to tegn}

    ::msgcat::mcset da {Delete} {Slet}

    ::msgcat::mcset da {Help...} {Hj�lp...}

    ::msgcat::mcset da HELPTEXT {

	Kevin's Hacky Input Method (KHIM)

	KHIM g�r det muligt at indtaste internationale tegn med et tastatur
	som ikke underst�tter disse. Dette fungerer uafh�ngigt af en
	best�ende indtast metode som styresystemet m�tte underst�tte.
	Det er t�nkt til at hj�lpe, hvis du ikke har kontrol over tilordningen
	p� dit tastatur og dog har brug for at indtaste tekst i andre sprog.
	
	For at benytte KHIM, vis kontrolpanelet (hvordan det g�res, afh�nger af
	dit program) og aktiver KHIM ved at afkrydse "Benyt KHIM". Du skal v�lge
	en taste	der kun sj�ldent benyttes p� dit tastatur og fastl�gge denne som
	"Compose" taste for at s�tte tegn sammen med. Tryk dertil p� knappen
	markeret med "Compose taste:" og tryk derefter p� den taste du �nsker at
	fastl�gge. Generellt skulle det ikke v�re den taste som normalt benyttes
	til at konstruere tegn med; denne taste vil fortsat opr�be din lokale
	systems indtast metode.

	N�r KHIM er aktiveret kan du indtaste internationale tegn i enhver
	kontrol der er konfigureret til at bruge KHIM ved at trykke p� den valgte
	"Compose" taste fulgt af to bestemte tegn. Rullelisten til venstre
	p� KHIM kontrolpanelet viser de tegnf�lger der st�r til r�dighed. Hvis
	du trykker "Compose" tasten to gange kommer der et vindue frem hvor
	du kan v�lge vilk�rlige symboler fra en unicode tabel. Du kan navigere
	rundt i selve tabellen ved enten at benytte mark�ren eller mark�rtasterne.
	Du kan udv�lge det markerede tegn ved at doppelt-klikke p� symbolet eller
	ved at trykke p� mellemrums-, enter- eller returtasten.

	Ny tegnf�lger kan defineres ved at indtaste en f�lge af to tegn i feltet
	markeret med "Indtast tegnf�lge" og det �nskede symbol i feltet markeret
	med "Tegn" og derefter trykke p� "�ndre". Du kan ogs� kopiere og inds�tte
	et symbol fra et andet program eller benytte "Unicode..." knappen
	(eller trykke "Compose" tasten to gange) for at hente et symbol fra
	tabellen med alle unicode koder. Tryk p� "Slet" knappen for at fjerne
	en tegnf�lge.

    }

    ::msgcat::mcset da {Input key sequence} {Indtast tegnf�lge}

    ::msgcat::mcset da {Insert Character} {Indtast et tegn}

    ::msgcat::mcset da {Invalid sequence} {Ugyldig tegnf�lge}

    ::msgcat::mcset da {Key sequences} {Tegnf�lger}

    ::msgcat::mcset da {KHIM Controls} {KHIM kontrolpanel}

    ::msgcat::mcset da {OK} {O.k.}

    ::msgcat::mcset da {Select code page:} {V�lg kode side:}

    ::msgcat::mcset da {SELECT COMPOSE KEY} [string map [list \n\t \n] {
	Tryk p� den taske
	du �nsker at bruge  
	som "Compose" taste.
    }]

    ::msgcat::mcset da {Unicode...} {Unicode...}

    ::msgcat::mcset da {Use KHIM} {Benyt KHIM}

}

# Local Variables:
# mode: tcl
# End:
