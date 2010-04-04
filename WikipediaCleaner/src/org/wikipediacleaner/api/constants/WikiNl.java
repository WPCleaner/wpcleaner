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
 * Configuration for <a href="http://nl.wikipedia.org/w/index.php">Dutch wikipedia</a>.
 */
class WikiNl {

  private final static String baseUrl = "http://nl.wikipedia.org/w/";

  final static String code     = "nl";
  final static String name     = "Nederlandstalige Wikipedia";

  final static String apiUrl   = baseUrl + "api.php";
  final static String queryUrl = baseUrl + "query.php";
  final static String indexUrl = baseUrl + "index.php";

  final static String helpUrl  = WikiEn.helpUrl;
  final static String helpLink = WikiEn.helpLink;

  final static ComponentOrientation orientation = ComponentOrientation.LEFT_TO_RIGHT;

  final static String message  = "Link naar doorverwijspagina aangepast. [[Wikipedia:Links naar doorverwijspagina's|Help mee!]]";

  final static String wikt     = null;
  final static TemplateMatch[] wiktMatches = new TemplateMatch[] {};

  final static String[] dabLinkTemplates = null;
  final static String[] needHelpTemplates = null;
  final static String[] helpRequestedTemplates = null;

  final static String dabList = "Wikipedia:Links_naar_doorverwijspagina's/data";
  final static TemplateMatch[] dabMatches = new TemplateMatch[] {};
  
  final static String checkWikiProject = "Wikipedia:CW";
}
