/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.api.constants;

import java.awt.ComponentOrientation;

import org.wikipediacleaner.api.data.TemplateMatch;
import org.wikipediacleaner.api.data.TemplateReplacement;


/**
 * Configuration for <a href="http://fr.wikipedia.org/w/index.php">French wikipedia</a>.
 */
class WikiFr {

  private final static String baseUrl = "http://fr.wikipedia.org/w/";

  final static String code     = "fr";
  final static String name     = "Wikipédia Français";

  final static String apiUrl   = baseUrl + "api.php";
  final static String queryUrl = baseUrl + "query.php";
  final static String indexUrl = baseUrl + "index.php";

  final static String helpUrl  = "http://fr.wikipedia.org/wiki/Utilisateur:NicoV/Wikipedia_Cleaner/Documentation";
  final static String helpLink = ":fr:Utilisateur:NicoV/Wikipedia Cleaner/Documentation";

  final static ComponentOrientation orientation = ComponentOrientation.LEFT_TO_RIGHT;

  final static String message  = "Correction des [[Projet:Liens vers les pages d'homonymie|liens vers les pages d'homonymie]]";

  final static String wikt     = "wikt";
  final static TemplateMatch[] wiktMatches = new TemplateMatch[] {
    new TemplateMatch("Wiktionnaire", "1", "{{pagename}}", true, false),
    new TemplateMatch("Wiktionnaire-2", "1,2", "", true, false),
  };

  final static String[] dabLinkTemplates = new String[] {
    "page h",
    "page h'",
    "page l",
  };
  final static String[] needHelpTemplates = new String[] {
    "lh",
  };
  final static String[] helpRequestedTemplates = new String[] {
    "Lien vers un homonyme",
  };

  final static String dabList = "Projet:Liens vers les pages d'homonymie/Export à traiter";
  final static TemplateMatch[] dabMatches = new TemplateMatch[] {
    new TemplateMatch("A", "1", "", false, false),
    new TemplateMatch("Article détaillé", "1,2,3,4,5,6,7,8,9,10", "", false, false),
    new TemplateMatch("Dn", "1", "", false, true),
    new TemplateMatch("Jumelage", "1,2", "", false, false, new TemplateReplacement[] {
        new TemplateReplacement("1", "1", "ville") } ),
    new TemplateMatch("Homon", "1,2,3,4,5", "{{PAGENAME}} (homonymie)", true, false),
    new TemplateMatch("Homonyme", "1,2,3,4,5", "{{PAGENAME}} (homonymie)", true, false),
    new TemplateMatch("Homophonie", "1,2,3,4,5,6,7", "{{PAGENAME}} (homophonie)", true, false),
    new TemplateMatch("Lh", "1", "", false, true),
    new TemplateMatch("Lien homonymie", "1", "", false, true),
    new TemplateMatch("Lien vers un homonyme", "1", "", false, true),
    new TemplateMatch("Loupe", "1,2,3,4,5,6,7,8,9,10", "", false, false),
    new TemplateMatch("Page h", "1", "", true, false),
    new TemplateMatch("Page h'", "1", "", true, false),
    new TemplateMatch("Page l", "1", "", true, false),
    new TemplateMatch("Paronyme", "1,2,3,4", "", true, false),
    new TemplateMatch("Voir homonyme", "1", "", true, false),
    new TemplateMatch("Voir homonymie", "1,2,3,4,5", "{{PAGENAME}} (homonymie)", true, false),
    new TemplateMatch("Voir homonymes", "1,2,3,4,5", "{{PAGENAME}} (homonymie)", true, false),
    new TemplateMatch("Voir homophone", "1,2,3,4,5,6,7", "{{PAGENAME}} (homophonie)", true, false),
    new TemplateMatch("Voir homophones", "1,2,3,4,5,6,7", "{{PAGENAME}} (homophonie)", true, false),
    new TemplateMatch("Voir paronymes", "1,2,3,4", "{{PAGENAME}} (paronymie)", true, false),
  };
  
  final static String checkWikiProject = "P:CS";
}
