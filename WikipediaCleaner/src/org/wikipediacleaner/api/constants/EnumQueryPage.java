/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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

import org.wikipediacleaner.i18n.GT;


/**
 * List types of special pages that can be used in QueryPage requests.
 */
public enum EnumQueryPage {

  ANCIENT_PAGES("Ancientpages", GT._No("Dormant pages")),
  BROKEN_REDIRECTS("BrokenRedirects", GT._No("Broken redirects")),
  DEAD_END_PAGES("Deadendpages", GT._No("Dead-end pages")),
  DISAMBIGUATIONS("Disambiguations", GT._No("Pages linking to disambiguation pages")),
  DOUBLE_REDIRECTS("DoubleRedirects", GT._No("Double redirects")),
  FEWEST_REVISIONS("Fewestrevisions", GT._No("Pages with the fewest revisions")),
  LIST_REDIRECTS("ListRedirects", GT._No("Redirects")),
  LONELY_PAGES("Lonelypages", GT._No("Orphaned pages")),
  LONG_PAGES("Longpages", GT._No("Long pages")),
  MOST_CATEGORIES("Mostcategories", GT._No("Pages with the most categories")),
  MOST_IMAGES("Mostimages", GT._No("Pages with the most files")),
  MOST_LINKED("Mostlinked", GT._No("Most linked-to pages")),
  MOST_LINKED_CATEGORIES("Mostlinkedcategories", GT._No("Most linked-to categories")),
  MOST_LINKED_TEMPLATES("Mostlinkedtemplates", GT._No("Most linked-to templates")),
  MOST_REVISIONS("Mostrevisions", GT._No("Pages with the most revisions")),
  SHORT_PAGES("Shortpages", GT._No("Short pages")),
  UNCATEGORIZED_CATEGORIES("Uncategorizedcategories", GT._No("Uncategorized categories")),
  UNCATEGORIZED_IMAGES("Uncategorizedimages", GT._No("Uncategorized files")),
  UNCATEGORIZED_PAGES("Uncategorizedpages", GT._No("Uncategorized pages")),
  UNCATEGORIZED_TEMPLATES("Uncategorizedtemplates", GT._No("Uncategorized templates")),
  UNUSED_CATEGORIES("Unusedcategories", GT._No("Unused categories")),
  UNUSED_IMAGES("Unusedimages", GT._No("Unused files")),
  UNUSED_TEMPLATES("Unusedtemplates", GT._No("Unused templates")),
  WANTED_CATEGORIES("Wantedcategories", GT._No("Wanted categories")),
  WANTED_FILES("Wantedfiles", GT._No("Wanted files")),
  WANTED_PAGES("Wantedpages", GT._No("Wanted pages")),
  WANTED_TEMPLATES("Wantedtemplates", GT._No("Wanted templates")),
  UNWATCHED_PAGES("Unwatchedpages", GT._No("Unwatched pages")),
  WITHOUT_INTERWIKI("Withoutinterwiki", GT._No("Pages without language links"));
  
  /**
   * Code for the special page.
   */
  private final String code;

  /**
   * Name of the special page.
   */
  private final String name;

  /**
   * @param code Code for the special page.
   * @param name Name of the special page.
   */
  EnumQueryPage(String code, String name) {
    this.code = code;
    this.name = name;
  }

  /**
   * @param code Code for the special page.
   * @return Type of special page requested.
   */
  public static EnumQueryPage findByCode(String code) {
    for (EnumQueryPage query : values()) {
      if (query.code.equals(code)) {
        return query;
      }
    }
    return null;
  }

  /**
   * @return Code for the special page.
   */
  public String getCode() {
    return code;
  }

  /**
   * @return Name of the special page.
   */
  public String getName() {
    return GT._(name);
  }
}
