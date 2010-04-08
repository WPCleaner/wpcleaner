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
 * Configuration for <a href="http://he.wikipedia.org/w/index.php">Hebrew wikipedia</a>.
 */
class WikiHe {

  private final static String baseUrl = "http://he.wikipedia.org/w/";

  final static String code     = "he";
  final static String name     = "ויקיפדיה העברית";

  final static String apiUrl   = baseUrl + "api.php";
  final static String queryUrl = baseUrl + "query.php";
  final static String indexUrl = baseUrl + "index.php";

  final static String helpUrl  = "http://he.wikipedia.org/wiki/%D7%9E%D7%A9%D7%AA%D7%9E%D7%A9:%D7%A9%D7%9E%D7%95%D7%91%D7%91%D7%94/%D7%94%D7%95%D7%A8%D7%90%D7%95%D7%AA_%D7%91%D7%A2%D7%91%D7%A8%D7%99%D7%AA";
  final static String helpLink = "%D7%9E%D7%A9%D7%AA%D7%9E%D7%A9:%D7%A9%D7%9E%D7%95%D7%91%D7%91%D7%94/%D7%94%D7%95%D7%A8%D7%90%D7%95%D7%AA_%D7%91%D7%A2%D7%91%D7%A8%D7%99%D7%AA";

  final static ComponentOrientation orientation = ComponentOrientation.RIGHT_TO_LEFT;

  final static String message  = "תיקון הפניה לדף פירושונים";

  final static String wikt     = null;
  final static TemplateMatch[] wiktMatches = new TemplateMatch[] {};

  final static String[] dabLinkTemplates = null;
  final static String[] needHelpTemplates = null;
  final static String[] helpRequestedTemplates = null;

  final static String dabList = "ויקיפדיה:מיזמי ויקיפדיה/מיזם דפי פירושונים";
  final static TemplateMatch[] dabMatches = new TemplateMatch[] {};
  
  final static String checkWikiProject = "ויקיפדיה:Check Wikipedia";
  final static String checkWikiTraduction = "ויקיפדיה:Check Wikipedia/Translation";
}
