{- | This plugin causes link URLs of the form wikiname!articlename to be
  treated as interwiki links.  So, for example,

> [The Emperor Palpatine](!Wookieepedia "Emperor Palpatine")

  links to the article on "Emperor Palpatine" in Wookieepedia
  (<http://starwars.wikia.com/wiki/Emperor_Palpatine>).

  This module also supports a shorter syntax, for when the link text
  is identical to the article name. Example:

> [Emperor Palpatine](!Wookieepedia)

  will link to the right place, same as the previous example.

  (Written by Gwern Branwen; put in public domain, 2009) -}

module Network.Gitit.Plugin.Interwiki (plugin) where

import Network.Gitit.Interface

import qualified Data.Map as M (fromList, lookup, Map)
import Network.URI (escapeURIString, isAllowedInURI, unEscapeString)

plugin :: Plugin
plugin = mkPageTransform convertInterwikiLinks

{- | A good interwiki link looks like '!Wookieepedia "Emperor Palpatine"'. So we check for a leading '!'.
     We strip it off, and now we have the canonical sitename (in this case, "Wookieepedia" and we look it up
     in our database.
     The database should return the URL for that site; we only need append the (escaped) article name to that,
     and we have the full URL! If there isn't one there, then we look back at the link-text for the article
     name; this is how we support the shortened syntax (see module description).
     If there isn't a leading '!', we get back a Nothing (the database doesn't know the site), we just return
     the Link unchanged. -}
convertInterwikiLinks :: Inline -> Inline
convertInterwikiLinks (Link attr ref (interwiki, article)) =
  case interwiki of
    ('!':interwiki') ->
        case M.lookup interwiki' interwikiMap of
                Just url  -> case article of
                                  "" -> Link attr ref (url ++ inlinesToURL ref, summary $ unEscapeString $ inlinesToURL ref)
                                  _  -> Link attr ref (interwikiurl article url, summary article)
                Nothing -> Link attr ref (interwiki, article)
            where -- 'http://starwars.wikia.com/wiki/Emperor_Palpatine'
                  interwikiurl a u = escapeURIString isAllowedInURI $ u ++ a
                  -- 'Wookieepedia: Emperor Palpatine'
                  summary a = interwiki' ++ ": " ++ a
    _ -> Link attr ref (interwiki, article)
convertInterwikiLinks x = x

-- | Large table of constants; this is a mapping from shortcuts to a URL. The URL can be used by
--   appending to it the article name (suitably URL-escaped, of course).
interwikiMap :: M.Map String String
interwikiMap = M.fromList $ wpInterwikiMap ++ customInterwikiMap

wpInterwikiMap, customInterwikiMap :: [(String, String)]
customInterwikiMap = [("Hackage", "http://hackage.haskell.org/package/"),
                      ("Hawiki", "http://haskell.org/haskellwiki/"),
                      ("Hayoo", "http://holumbus.fh-wedel.de/hayoo/hayoo.html#0:"),
                      ("Hoogle", "http://www.haskell.org/hoogle/?hoogle=")]

-- This mapping is derived from <https://secure.wikimedia.org/wikipedia/meta/wiki/Interwiki_map>
-- as of 6:12 PM, 6 February 2013.
wpInterwikiMap = [ ("AbbeNormal", "http://johnabbe.wagn.org/"),
                 ("Acronym", "http://www.acronymfinder.com/af-query.asp?String=exact&Acronym="),
                 ("Advisory", "http://advisory.wikimedia.org/wiki/"),
                 ("Advogato", "http://www.advogato.org/"),
                 ("Aew", "http://wiki.arabeyes.org/"),
                 ("AllWiki", "http://allwiki.com/index.php/"),
                 ("Appropedia", "http://www.appropedia.org/"),
                 ("AquariumWiki", "http://www.theaquariumwiki.com/"),
                 ("AspieNetWiki", "http://aspie.mela.de/index.php/"),
                 ("AtmWiki", "http://www.otterstedt.de/wiki/index.php/"),
                 ("BCNbio", "http://historiapolitica.bcn.cl/resenas_parlamentarias/wiki/"),
                 ("BLW", "http://britainloveswikipedia.org/wiki/"),
                 ("BattlestarWiki", "http://en.battlestarwiki.org/wiki/"),
                 ("BibleWiki", "http://bible.tmtm.com/wiki/"),
                 ("BluWiki", "http://bluwiki.com/go/"),
                 ("Botwiki", "http://botwiki.sno.cc/wiki/"),
                 ("Boxrec", "http://www.boxrec.com/media/index.php?"),
                 ("BrickWiki", "http://lego.wikia.com/index.php?title="),
                 ("Bytesmiths", "http://www.Bytesmiths.com/wiki/"),
                 ("C2", "http://c2.com/cgi/wiki?"),
                 ("C2find", "http://c2.com/cgi/wiki?FindPage&value="),
                 ("CKWiss", "http://ck-wissen.de/ckwiki/index.php?title="),
                 ("Cache", "http://www.google.com/search?q=cache:"),
                 ("CellWiki", "http://cell.wikia.com/wiki/"),
                 ("CentralWikia", "http://community.wikia.com/wiki/"),
                 ("ChEJ", "http://esperanto.blahus.cz/cxej/vikio/index.php/"),
                 ("ChoralWiki", "http://www.cpdl.org/wiki/index.php/"),
                 ("Citizendium", "http://en.citizendium.org/wiki/"),
                 ("Comixpedia", "http://www.comixpedia.org/index.php/"),
                 ("Commons", "http://commons.wikimedia.org/wiki/"),
                 ("CommunityScheme", "http://community.schemewiki.org/?c=s&key="),
                 ("CommunityWiki", "http://www.communitywiki.org/"),
                 ("CorpKnowPedia", "http://corpknowpedia.org/wiki/index.php/"),
                 ("CrazyHacks", "http://www.crazy-hacks.org/wiki/index.php?title="),
                 ("CreativeCommons", "http://www.creativecommons.org/licenses/"),
                 ("CreativeCommonsWiki", "http://wiki.creativecommons.org/"),
                 ("CreaturesWiki", "http://creatures.wikia.com/wiki/"),
                 ("CxEJ", "http://esperanto.blahus.cz/cxej/vikio/index.php/"),
                 ("DCDatabase", "http://dc.wikia.com/"),
                 ("DCMA", "http://www.christian-morgenstern.de/dcma/"),
                 ("DOI", "http://dx.doi.org/"),
                 ("DRAE", "http://lema.rae.es/drae/?val="),
                 ("DWJWiki", "http://www.suberic.net/cgi-bin/dwj/wiki.cgi?"),
                 ("Dcc", "http://www.dccwiki.com/"),
                 ("DejaNews", "http://www.deja.com/=dnc/getdoc.xp?AN="),
                 ("Delicious", "http://www.delicious.com/tag/"),
                 ("Devmo", "https://developer.mozilla.org/en/docs/"),
                 ("Dict", "http://www.dict.org/bin/Dict?Database=*&Form=Dict1&Strategy=*&Query="),
                 ("Dictionary", "http://www.dict.org/bin/Dict?Database=*&Form=Dict1&Strategy=*&Query="),
                 ("Disinfopedia", "http://www.sourcewatch.org/wiki.phtml?title="),
                 ("DocBook", "http://wiki.docbook.org/topic/"),
                 ("Donate", "http://donate.wikimedia.org/wiki/"),
                 ("Dreamhost", "http://wiki.dreamhost.com/index.php/"),
                 ("DrumCorpsWiki", "http://www.drumcorpswiki.com/index.php/"),
                 ("ELibre", "http://enciclopedia.us.es/index.php/"),
                 ("EcoReality", "http://www.EcoReality.org/wiki/"),
                 ("EcxeI", "http://www.ikso.net/cgi-bin/wiki.pl?"),
                 ("EmacsWiki", "http://www.emacswiki.org/cgi-bin/wiki.pl?"),
                 ("Encyc", "http://encyc.org/wiki/"),
                 ("EnergieWiki", "http://www.netzwerk-energieberater.de/wiki/index.php/"),
                 ("EoKulturCentro", "http://esperanto.toulouse.free.fr/nova/wikini/wakka.php?wiki="),
                 ("Etherpad", "http://etherpad.wikimedia.org/"),
                 ("Ethnologue", "http://www.ethnologue.com/show_language.asp?code="),
                 ("EthnologueFamily", "http://www.ethnologue.com/show_family.asp?subid="),
                 ("EvoWiki", "http://wiki.cotch.net/index.php/"),
                 ("Exotica", "http://www.exotica.org.uk/wiki/"),
                 ("EĉeI", "http://www.ikso.net/cgi-bin/wiki.pl?"),
                 ("FanimutationWiki", "http://wiki.animutationportal.com/index.php/"),
                 ("FinalEmpire", "http://final-empire.sourceforge.net/cgi-bin/wiki.pl?"),
                 ("FinalFantasy", "http://finalfantasy.wikia.com/wiki/"),
                 ("Finnix", "http://www.finnix.org/"),
                 ("FlickrPhoto", "http://www.flickr.com/photo.gne?id="),
                 ("FlickrUser", "http://www.flickr.com/people/"),
                 ("FloralWIKI", "http://www.floralwiki.co.uk/wiki/"),
                 ("FlyerWiki-de", "http://de.flyerwiki.net/index.php/"),
                 ("Foldoc", "http://foldoc.org/"),
                 ("ForthFreak", "http://wiki.forthfreak.net/index.cgi?"),
                 ("Foundation", "http://wikimediafoundation.org/wiki/"),
                 ("FoxWiki", "http://fox.wikis.com/wc.dll?Wiki~"),
                 ("FreeBSDman", "http://www.FreeBSD.org/cgi/man.cgi?apropos=1&query="),
                 ("FreeBio", "http://freebiology.org/wiki/"),
                 ("FreeCultureWiki", "http://wiki.freeculture.org/index.php/"),
                 ("FreeFeel", "http://freefeel.org/wiki/"),
                 ("Freedomdefined", "http://freedomdefined.org/"),
                 ("FreekiWiki", "http://wiki.freegeek.org/index.php/"),
                 ("Freenode", "http://ganfyd.org/index.php?title="),
                 ("Gardenology", "http://www.gardenology.org/wiki/"),
                 ("GaussWiki", "http://gauss.ffii.org/"),
                 ("GenWiki", "http://wiki.genealogy.net/index.php/"),
                 ("Gentoo-Wiki", "http://gentoo-wiki.com/"),
                 ("Gerrit", "https://gerrit.wikimedia.org/r/"),
                 ("Git", "https://gerrit.wikimedia.org/r/gitweb?p=mediawiki/a=log;h=refs/heads/master;"),
                 ("GlobalVoices", "http://cyber.law.harvard.edu/dyn/globalvoices/wiki/"),
                 ("GlossarWiki", "http://glossar.hs-augsburg.de/"),
                 ("GlossaryWiki", "http://glossary.hs-augsburg.de/"),
                 ("Google", "http://www.google.com/search?q="),
                 ("GoogleDefine", "http://www.google.com/search?q=define:"),
                 ("GoogleGroups", "http://groups.google.com/groups?q="),
                 ("GotAMac", "http://www.got-a-mac.org/"),
                 ("GreatLakesWiki", "http://greatlakeswiki.org/index.php/"),
                 ("GuildWarsWiki", "http://www.wiki.guildwars.com/wiki/"),
                 ("Guildwiki", "http://guildwars.wikia.com/wiki/"),
                 ("H2Wiki", "http://halowiki.net/p/"),
                 ("HRFWiki", "http://fanstuff.hrwiki.org/index.php/"),
                 ("HRWiki", "http://www.hrwiki.org/index.php/"),
                 ("HammondWiki", "http://www.dairiki.org/HammondWiki/index.php3?"),
                 ("HupWiki", "http://wiki.hup.hu/index.php/"),
                 ("IMDbCharacter", "http://www.imdb.com/character/ch/"),
                 ("IMDbCompany", "http://www.imdb.com/company/co/"),
                 ("IMDbName", "http://www.imdb.com/name/nm/"),
                 ("IMDbTitle", "http://www.imdb.com/title/tt/"),
                 ("IRC", "http://www.sil.org/iso639-3/documentation.asp?id="),
                 ("ISSN", "http://www.worldcat.org/issn/"),
                 ("Incubator", "http://incubator.wikimedia.org/wiki/"),
                 ("Infosecpedia", "http://infosecpedia.org/wiki/"),
                 ("Infosphere", "http://theinfosphere.org/"),
                 ("Iuridictum", "http://iuridictum.pecina.cz/w/"),
                 ("JEFO", "http://esperanto-jeunes.org/wiki/"),
                 ("JSTOR", "http://www.jstor.org/journals/"),
                 ("JamesHoward", "http://jameshoward.us/"),
                 ("JavaNet", "http://wiki.java.net/bin/view/Main/"),
                 ("Javapedia", "http://wiki.java.net/bin/view/Javapedia/"),
                 ("JiniWiki", "http://www.cdegroot.com/cgi-bin/jini?"),
                 ("Jira", "https://jira.toolserver.org/browse/"),
                 ("JspWiki", "http://www.ecyrd.com/JSPWiki/Wiki.jsp?page="),
                 ("Kamelo", "http://kamelopedia.mormo.org/index.php/"),
                 ("Karlsruhe", "http://ka.stadtwiki.net/"),
                 ("KerimWiki", "http://wiki.oxus.net/"),
                 ("KinoWiki", "http://kino.skripov.com/index.php/"),
                 ("KmWiki", "http://kmwiki.wikispaces.com/"),
                 ("KontuWiki", "http://kontu.merri.net/wiki/"),
                 ("KoslarWiki", "http://wiki.koslar.de/index.php/"),
                 ("Kpopwiki", "http://www.kpopwiki.com/"),
                 ("LISWiki", "http://liswiki.org/wiki/"),
                 ("LQWiki", "http://wiki.linuxquestions.org/wiki/"),
                 ("LinguistList", "http://linguistlist.org/forms/langs/LLDescription.cfm?code="),
                 ("LinuxWiki", "http://www.linuxwiki.de/"),
                 ("LinuxWikiDe", "http://www.linuxwiki.de/"),
                 ("LiteratePrograms", "http://en.literateprograms.org/"),
                 ("Livepedia", "http://www.livepedia.gr/index.php?title="),
                 ("Lojban", "http://www.lojban.org/tiki/tiki-index.php?page="),
                 ("Lostpedia", "http://lostpedia.wikia.com/wiki/"),
                 ("LugKR", "http://lug-kr.sourceforge.net/cgi-bin/lugwiki.pl?"),
                 ("Luxo", "http://toolserver.org/~luxo/contributions/contributions.php?user="),
                 ("MW", "http://www.mediawiki.org/wiki/"),
                 ("MWOD", "http://www.merriam-webster.com/cgi-bin/dictionary?book=Dictionary&va="),
                 ("MWOT", "http://www.merriam-webster.com/cgi-bin/thesaurus?book=Thesaurus&va="),
                 ("Mail", "https://lists.wikimedia.org/mailman/listinfo/"),
                 ("Mariowiki", "http://www.mariowiki.com/"),
                 ("MarvelDatabase", "http://www.marveldatabase.com/wiki/index.php/"),
                 ("MeatBall", "http://meatballwiki.org/wiki/"),
                 ("MediaWikiWiki", "http://www.mediawiki.org/wiki/"),
                 ("MediaZilla", "https://bugzilla.wikimedia.org/"),
                 ("MemoryAlpha", "http://memory-alpha.org/wiki/"),
                 ("MetaWiki", "http://sunir.org/apps/meta.pl?"),
                 ("MetaWikiPedia", "http://meta.wikimedia.org/wiki/"),
                 ("Mineralienatlas", "http://www.mineralienatlas.de/lexikon/index.php/"),
                 ("MoinMoin", "http://moinmo.in/"),
                 ("Monstropedia", "http://www.monstropedia.org/?title="),
                 ("MosaPedia", "http://mosapedia.de/wiki/index.php/"),
                 ("MozCom", "http://mozilla.wikia.com/wiki/"),
                 ("MozillaWiki", "https://wiki.mozilla.org/"),
                 ("MozillaZineKB", "http://kb.mozillazine.org/"),
                 ("MusicBrainz", "http://musicbrainz.org/doc/"),
                 ("NARA", "http://research.archives.gov/description/"),
                 ("NKcells", "http://www.nkcells.info/wiki/index.php/"),
                 ("NoSmoke", "http://no-smok.net/nsmk/"),
                 ("Nost", "http://nostalgia.wikipedia.org/wiki/"),
                 ("OEIS", "http://oeis.org/"),
                 ("OLPC", "http://wiki.laptop.org/go/"),
                 ("OSI", "http://wiki.tigma.ee/index.php/"),
                 ("OSMwiki", "http://wiki.openstreetmap.org/wiki/"),
                 ("OTRS", "https://ticket.wikimedia.org/otrs/index.pl?Action=AgentTicketZoom&TicketID="),
                 ("OTRSwiki", "http://otrs-wiki.wikimedia.org/wiki/"),
                 ("OldWikisource", "http://wikisource.org/wiki/"),
                 ("OneLook", "http://www.onelook.com/?ls=b&w="),
                 ("OpenFacts", "http://openfacts.berlios.de/index-en.phtml?title="),
                 ("OpenWetWare", "http://openwetware.org/wiki/"),
                 ("OpenWiki", "http://openwiki.com/?"),
                 ("Openlibrary", "http://openlibrary.org/"),
                 ("Openstreetmap", "http://wiki.openstreetmap.org/wiki/"),
                 ("Opera7Wiki", "http://operawiki.info/"),
                 ("OrganicDesign", "http://www.organicdesign.co.nz/"),
                 ("OrthodoxWiki", "http://orthodoxwiki.org/"),
                 ("OurMedia", "https://www.socialtext.net/ourmedia/index.cgi?"),
                 ("Outreach", "http://outreach.wikimedia.org/wiki/"),
                 ("OutreachWiki", "http://outreach.wikimedia.org/wiki/"),
                 ("PHWiki", "http://wiki.pocketheaven.com/"),
                 ("PMEG", "http://www.bertilow.com/pmeg/"),
                 ("Panawiki", "http://wiki.alairelibre.net/index.php?title="),
                 ("PatWIKI", "http://gauss.ffii.org/"),
                 ("PerlNet", "http://perl.net.au/wiki/"),
                 ("PersonalTelco", "http://www.personaltelco.net/"),
                 ("PhpWiki", "http://phpwiki.sourceforge.net/phpwiki/index.php?"),
                 ("PlanetMath", "http://planetmath.org/?op=getobj&from=objects&id="),
                 ("PyWiki", "http://c2.com/cgi/wiki?"),
                 ("PythonInfo", "http://www.python.org/cgi-bin/moinmoin/"),
                 ("PythonWiki", "http://www.pythonwiki.de/"),
                 ("Quality", "http://quality.wikimedia.org/wiki/"),
                 ("RFC", "http://tools.ietf.org/html/rfc"),
                 ("ReVo", "http://purl.org/NET/voko/revo/art/.html"),
                 ("ReutersWiki", "http://glossary.reuters.com/index.php/"),
                 ("RheinNeckar", "http://rhein-neckar-wiki.de/"),
                 ("RoWiki", "http://wiki.rennkuckuck.de/index.php/"),
                 ("RoboWiki", "http://robowiki.net/?"),
                 ("SLWiki", "http://wiki.secondlife.com/wiki/"),
                 ("SMikipedia", "http://www.smiki.de/"),
                 ("SVGWiki", "http://wiki.svg.org/index.php/"),
                 ("Scholar", "http://scholar.google.com/scholar?q="),
                 ("SchoolsWP", "http://schools-wikipedia.org/wiki/"),
                 ("Scores", "http://imslp.org/wiki/"),
                 ("Scoutwiki", "http://en.scoutwiki.org/"),
                 ("Scramble", "http://www.scramble.nl/wiki/index.php?title="),
                 ("SeaPig", "http://www.seapig.org/"),
                 ("SeattleWiki", "http://seattlewiki.org/wiki/"),
                 ("SeattleWireless", "http://seattlewireless.net/?"),
                 ("SenseisLibrary", "http://senseis.xmp.net/?"),
                 ("Slashdot", "http://slashdot.org/article.pl?sid="),
                 ("SourceForge", "http://sourceforge.net/"),
                 ("Species", "http://species.wikimedia.org/wiki/"),
                 ("Squeak", "http://wiki.squeak.org/squeak/"),
                 ("Stewardry", "http://toolserver.org/~pathoschild/stewardry/?wiki="),
                 ("Strategy", "http://strategy.wikimedia.org/wiki/"),
                 ("StrategyWiki", "http://strategywiki.org/wiki/"),
                 ("Sulutil", "http://toolserver.org/~quentinv57/sulinfo/"),
                 ("SwinBrain", "http://mercury.it.swin.edu.au/swinbrain/index.php/"),
                 ("SwingWiki", "http://www.swingwiki.org/"),
                 ("Swtrain", "http://train.spottingworld.com/"),
                 ("TESOLTaiwan", "http://www.tesol-taiwan.org/wiki/index.php/"),
                 ("TMBW", "http://tmbw.net/wiki/"),
                 ("TMwiki", "http://www.EasyTopicMaps.com/?page="),
                 ("TVIV", "http://tviv.org/wiki/"),
                 ("TVtropes", "http://www.tvtropes.org/pmwiki/pmwiki.php/Main/"),
                 ("TWiki", "http://twiki.org/cgi-bin/view/"),
                 ("TabWiki", "http://www.tabwiki.com/index.php/"),
                 ("Tavi", "http://tavi.sourceforge.net/"),
                 ("TclersWiki", "http://wiki.tcl.tk/"),
                 ("Technorati", "http://www.technorati.com/search/"),
                 ("Tenwiki", "http://ten.wikipedia.org/wiki/"),
                 ("Test2wiki", "//test2.wikipedia.org/wiki/"),
                 ("Testwiki", "http://test.wikipedia.org/wiki/"),
                 ("Thelemapedia", "http://www.thelemapedia.org/index.php/"),
                 ("Theopedia", "http://www.theopedia.com/"),
                 ("ThinkWiki", "http://www.thinkwiki.org/wiki/"),
                 ("TibiaWiki", "http://tibia.erig.net/"),
                 ("Ticket", "https://ticket.wikimedia.org/otrs/index.pl?Action=AgentTicketZoom&TicketNumber="),
                 ("TmNet", "http://www.technomanifestos.net/?"),
                 ("Tools", "http://toolserver.org/"),
                 ("Turismo", "http://www.tejo.org/turismo/"),
                 ("TyvaWiki", "http://www.tyvawiki.org/wiki/"),
                 ("USEJ", "http://www.tejo.org/usej/"),
                 ("Uncyclopedia", "http://uncyclopedia.org/wiki/"),
                 ("Unreal", "http://wiki.beyondunreal.com/wiki/"),
                 ("Urbandict", "http://www.urbandictionary.com/define.php?term="),
                 ("UseMod", "http://www.usemod.com/cgi-bin/wiki.pl?"),
                 ("VIAF", "http://viaf.org/viaf/"),
                 ("VKoL", "http://kol.coldfront.net/thekolwiki/index.php/"),
                 ("VLOS", "http://www.thuvienkhoahoc.com/tusach/"),
                 ("Vinismo", "http://vinismo.com/en/"),
                 ("VoIPinfo", "http://www.voip-info.org/wiki/view/"),
                 ("WLUG", "http://www.wlug.org.nz/"),
                 ("WMDEblog", "//blog.wikimedia.de/"),
                 ("WMF", "http://wikimediafoundation.org/wiki/"),
                 ("WMFblog", "http://blog.wikimedia.org/"),
                 ("Webisodes", "http://www.webisodes.org/"),
                 ("Wiki", "http://c2.com/cgi/wiki?"),
                 ("WikiChristian", "http://www.wikichristian.org/index.php?title="),
                 ("WikiF1", "http://www.wikif1.org/"),
                 ("WikiFur", "http://en.wikifur.com/wiki/"),
                 ("WikiIndex", "http://wikiindex.org/"),
                 ("WikiLemon", "http://wiki.illemonati.com/"),
                 ("WikiMac-de", "http://apfelwiki.de/wiki/Main/"),
                 ("WikiSkripta", "http://www.wikiskripta.eu/index.php/"),
                 ("WikiTI", "http://wikiti.denglend.net/index.php?title="),
                 ("WikiTravel", "http://wikitravel.org/en/"),
                 ("WikiTree", "http://wikitree.org/index.php?title="),
                 ("WikiWeet", "http://wikiweet.nl/wiki/"),
                 ("WikiWikiWeb", "http://c2.com/cgi/wiki?"),
                 ("Wikia", "http://www.wikia.com/wiki/c:"),
                 ("WikiaSite", "http://www.wikia.com/wiki/c:"),
                 ("Wikibooks", "http://en.wikibooks.org/wiki/"),
                 ("Wikichat", "http://www.wikichat.org/"),
                 ("Wikicities", "http://www.wikia.com/wiki/"),
                 ("Wikicity", "http://www.wikia.com/wiki/c:"),
                 ("Wikidata", "//wikidata.org/wiki/"),
                 ("Wikilivres", "http://wikilivres.ca/wiki/"),
                 ("Wikimedia", "http://wikimediafoundation.org/wiki/"),
                 ("Wikinews", "http://en.wikinews.org/wiki/"),
                 ("Wikinfo", "http://www.wikinfo.org/index.php/"),
                 ("Wikinvest", "http://www.wikinvest.com/"),
                 ("Wikipaltz", "http://www.wikipaltz.com/wiki/"),
                 ("Wikipedia", "http://en.wikipedia.org/wiki/"),
                 ("WikipediaWikipedia", "http://en.wikipedia.org/wiki/Wikipedia:"),
                 ("Wikiquote", "http://en.wikiquote.org/wiki/"),
                 ("Wikischool", "http://www.wikischool.de/wiki/"),
                 ("Wikisource", "http://en.wikisource.org/wiki/"),
                 ("Wikispecies", "http://species.wikimedia.org/wiki/"),
                 ("Wikispot", "http://wikispot.org/?action=gotowikipage&v="),
                 ("Wikitech", "http://wikitech.wikimedia.org/view/"),
                 ("Wikiversity", "http://en.wikiversity.org/wiki/"),
                 ("Wikivoyage", "//en.wikivoyage.org/wiki/"),
                 ("Wiktionary", "http://en.wiktionary.org/wiki/"),
                 ("Wipipedia", "http://www.londonfetishscene.com/wipi/index.php/"),
                 ("Wm2005", "http://wikimania2005.wikimedia.org/wiki/"),
                 ("Wm2006", "http://wikimania2006.wikimedia.org/wiki/"),
                 ("Wm2007", "http://wikimania2007.wikimedia.org/wiki/"),
                 ("Wm2008", "http://wikimania2008.wikimedia.org/wiki/"),
                 ("Wm2009", "http://wikimania2009.wikimedia.org/wiki/"),
                 ("Wm2010", "http://wikimania2010.wikimedia.org/wiki/"),
                 ("Wm2011", "http://wikimania2011.wikimedia.org/wiki/"),
                 ("Wm2011", "http://wikimania2011.wikimedia.org/wiki/"),
                 ("Wm2013", "//wikimania2013.wikimedia.org/wiki/"),
                 ("Wmania", "http://wikimania.wikimedia.org/wiki/"),
                 ("Wmteam", "http://wikimaniateam.wikimedia.org/wiki/"),
                 ("WoWWiki", "http://www.wowwiki.com/"),
                 ("Wookieepedia", "http://starwars.wikia.com/wiki/"),
                 ("Wqy", "http://wqy.sourceforge.net/cgi-bin/index.cgi?"),
                 ("WurmPedia", "http://www.wurmonline.com/wiki/index.php/"),
                 ("ZRHwiki", "http://www.zrhwiki.ch/wiki/"),
                 ("ZUM", "http://wiki.zum.de/"),
                 ("ZWiki", "http://www.zwiki.org/"),
                 ("arXiv", "http://arxiv.org/abs/"),
                 ("betawiki", "http://translatewiki.net/wiki/"),
                 ("betawikiversity", "http://beta.wikiversity.org/wiki/"),
                 ("bugzilla", "https://bugzilla.wikimedia.org/show_bug.cgi?id="),
                 ("bulba", "http://bulbapedia.bulbagarden.net/wiki/"),
                 ("buzztard", "http://buzztard.org/index.php/"),
                 ("comune", "http://rete.comuni-italiani.it/wiki/"),
                 ("dbdump", "http://download.wikimedia.org//latest/"),
                 ("distributedproofreaders", "http://www.pgdp.net/wiki/"),
                 ("distributedproofreadersca", "http://www.pgdpcanada.net/wiki/index.php/"),
                 ("dmoz", "http://www.dmoz.org/"),
                 ("dmozs", "http://www.dmoz.org/cgi-bin/search?search="),
                 ("doom_wiki", "http://doom.wikia.com/wiki/"),
                 ("download", "http://download.wikimedia.org/"),
                 ("gutenberg", "http://www.gutenberg.org/etext/"),
                 ("gutenbergwiki", "http://www.gutenberg.org/wiki/"),
                 ("heroeswiki", "http://heroeswiki.com/"),
                 ("infoAnarchy", "http://www.infoanarchy.org/en/"),
                 ("labsconsole", "https://labsconsole.wikimedia.org/wiki/"),
                 ("lyricwiki", "http://lyrics.wikia.com/"),
                 ("mailarchive", "http://lists.wikimedia.org/pipermail/"),
                 ("nostalgia", "http://nostalgia.wikipedia.org/wiki/"),
                 ("psycle", "http://psycle.sourceforge.net/wiki/"),
                 ("pyrev", "http://www.mediawiki.org/wiki/Special:Code/pywikipedia/"),
                 ("qcwiki", "http://wiki.quantumchemistry.net/index.php/"),
                 ("rev", "http://www.mediawiki.org/wiki/Special:Code/MediaWiki/"),
                 ("rtfm", "http://s23.org/wiki/"),
                 ("securewikidc", "https://secure.wikidc.org/"),
                 ("semantic-mw", "http://www.semantic-mediawiki.org/wiki/"),
                 ("silcode", "http://www.sil.org/iso639-3/documentation.asp?id="),
                 ("spcom", "http://spcom.wikimedia.org/wiki/"),
                 ("stats", "http://stats.wikimedia.org/"),
                 ("svn", "http://svn.wikimedia.org/viewvc/mediawiki/?view=log"),
                 ("translatewiki", "http://translatewiki.net/wiki/"),
                 ("tswiki", "http://wiki.toolserver.org/view/"),
                 ("usability", "http://usability.wikimedia.org/wiki/"),
                 ("wg", "http://wg.en.wikipedia.org/wiki/"),
                 ("wikiHow", "http://www.wikihow.com/"),
                 ("wikisophia", "http://wikisophia.org/index.php?title="),
                 ("wmar", "http://www.wikimedia.org.ar/wiki/"),
                 ("wmau", "http://wikimedia.org.au/wiki/"),
                 ("wmbd", "//bd.wikimedia.org/wiki/"),
                 ("wmbe", "http://be.wikimedia.org/wiki/"),
                 ("wmbr", "http://br.wikimedia.org/wiki/"),
                 ("wmca", "http://wikimedia.ca/wiki/"),
                 ("wmch", "http://www.wikimedia.ch/"),
                 ("wmcl", "http://www.wikimediachile.cl/index.php?title="),
                 ("wmco", "//co.wikimedia.org/wiki/"),
                 ("wmcz", "http://meta.wikimedia.org/wiki/Wikimedia_Czech_Republic/"),
                 ("wmdc", "http://wikimediadc.org/wiki/"),
                 ("wmde", "http://wikimedia.de/wiki/"),
                 ("wmdk", "//dk.wikimedia.org/wiki/"),
                 ("wmee", "//et.wikimedia.org/wiki/"),
                 ("wmet", "//et.wikimedia.org/wiki/"),
                 ("wmfi", "http://fi.wikimedia.org/wiki/"),
                 ("wmfr", "http://wikimedia.fr/"),
                 ("wmhk", "http://wikimedia.hk/index.php/"),
                 ("wmhu", "http://wiki.media.hu/wiki/"),
                 ("wmid", "http://www.wikimedia.or.id/wiki/"),
                 ("wmil", "http://www.wikimedia.org.il/"),
                 ("wmin", "http://wiki.wikimedia.in/"),
                 ("wmit", "http://wiki.wikimedia.it/wiki/"),
                 ("wmke", "http://wikimedia.or.ke/"),
                 ("wmmk", "//mk.wikimedia.org/wiki/"),
                 ("wmmx", "http://mx.wikimedia.org/wiki/"),
                 ("wmnl", "http://nl.wikimedia.org/wiki/"),
                 ("wmno", "http://no.wikimedia.org/wiki/"),
                 ("wmnyc", "http://nyc.wikimedia.org/wiki/"),
                 ("wmpa-us", "http://pa.us.wikimedia.org/wiki/"),
                 ("wmph", "http://www.wikimedia.ph/wmph/index.php?title="),
                 ("wmpl", "http://pl.wikimedia.org/wiki/"),
                 ("wmpt", "http://wikimedia.pt/"),
                 ("wmrs", "http://rs.wikimedia.org/wiki/"),
                 ("wmru", "http://ru.wikimedia.org/wiki/"),
                 ("wmse", "http://se.wikimedia.org/wiki/"),
                 ("wmtr", "//tr.wikimedia.org/wiki/"),
                 ("wmtw", "http://wikimedia.tw/wiki/index.php5/"),
                 ("wmua", "//ua.wikimedia.org/wiki/"),
                 ("wmuk", "http://uk.wikimedia.org/wiki/"),
                 ("wmve", "http://wikimedia.org.ve/index.php/"),
                 ("wmza", "http://wikimedia.org.za/wiki/"),
                 ("ĈEJ", "http://esperanto.blahus.cz/cxej/vikio/index.php/"),
                 ("ZZZ", "http://wiki.zzz.ee/index.php/") ]
