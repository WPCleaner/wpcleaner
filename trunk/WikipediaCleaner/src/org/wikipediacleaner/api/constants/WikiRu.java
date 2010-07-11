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
 * Configuration for <a href="http://ru.wikipedia.org/w/index.php">Russian wikipedia</a>.
 */
class WikiRu {

  private final static String baseUrl = "http://ru.wikipedia.org/w/";

  final static String code     = "ru";
  final static String name     = "Википедия";

  final static String apiUrl   = baseUrl + "api.php";
  final static String queryUrl = baseUrl + "query.php";
  final static String indexUrl = baseUrl + "index.php";

  final static ComponentOrientation orientation = ComponentOrientation.LEFT_TO_RIGHT;

  final static String configuration = "Участник:NicoV/WikiCleanerConfiguration";

  final static TemplateMatch[] dabMatches = new TemplateMatch[] {
    new TemplateMatch("Другие значения", "1", "{{PAGENAME}} (значения)", true, false),
    new TemplateMatch("Другое значение", "1", "{{PAGENAME}}", true, false),
    new TemplateMatch("Однофамильцы", "1", "{{PAGENAME}}", true, false)
  };
}
