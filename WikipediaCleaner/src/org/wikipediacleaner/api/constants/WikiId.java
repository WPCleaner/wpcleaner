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


/**
 * Configuration for <a href="http://id.wikipedia.org/w/index.php">Indonesian wikipedia</a>.
 */
class WikiId {

  private final static String baseUrl = "http://id.wikipedia.org/w/";

  final static String code     = "id";
  final static String name     = "Indonesian Wikipedia";

  final static String apiUrl   = baseUrl + "api.php";
  final static String queryUrl = baseUrl + "query.php";
  final static String indexUrl = baseUrl + "index.php";

  final static String helpUrl  = WikiEn.helpUrl;
  final static String helpLink = WikiEn.helpLink;

  final static ComponentOrientation orientation = ComponentOrientation.LEFT_TO_RIGHT;

  final static String message  = "ProyekWiki disambiguasi - [[Wikipedia:ProyekWiki Disambiguasi|Mari bergabung !]]";

  final static String wikt     = null;
  final static TemplateMatch[] wiktMatches = new TemplateMatch[] {};

  final static String[] dabLinkTemplates = new String[] { "Templat:Disambig info" };
  final static String[] needHelpTemplates = null;
  final static String[] helpRequestedTemplates = null;

  final static String[] dabList = { "Wikipedia:ProyekWiki Disambiguasi" };
  final static TemplateMatch[] dabMatches = new TemplateMatch[] {
    new TemplateMatch("Disambig", "1", "", false, false),
    new TemplateMatch("Disambig info", "1", "", true, false),
    new TemplateMatch("Disambig karya", "1", "", false, false),
    new TemplateMatch("Disambig nama", "1", "", false, false),
    new TemplateMatch("Disambig tempat", "1", "", false, false),
  };
  
  final static String checkWikiProject = "Wikipedia:ProyekWiki Cek Wikipedia";
  final static String checkWikiTraduction = "Wikipedia:ProyekWiki Cek Wikipedia/Terjemahan";
}
