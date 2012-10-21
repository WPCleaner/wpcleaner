/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
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

package org.wikipediacleaner.api.data;

import java.util.List;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.utils.Configuration;


/**
 * 
 */
public class DataManager {

  /**
   * @param wikipedia Wikipedia.
   * @param title Page title.
   * @param revisionId Revision id.
   * @param knownPages Already known pages.
   * @return The requested page.
   */
  public static Page getPage(
      EnumWikipedia wikipedia,
      String title, String revisionId,
      List<Page> knownPages) {

    // Check in the known pages
    if (knownPages != null) {
      for (Page page : knownPages) {
        if ((page != null) &&
            (page.getWikipedia() == wikipedia) &&
            (Page.areSameTitle(page.getTitle(), title)) &&
            ((revisionId == null) || (revisionId.equals(page.getRevisionId().toString())))) {
          return page;
        }
      }
    }

    // Retrieve page
    Page page = new Page(wikipedia, title);
    page.setRevisionId(revisionId);

    // Manage namespace
    if (page.getTitle() != null) {
      int colonIndex = page.getTitle().indexOf(':');
      if (colonIndex > 0) {
        String namespaceText = page.getTitle().substring(0, colonIndex);
        List<Namespace> namespaces = wikipedia.getWikiConfiguration().getNamespaces();
        if (namespaces != null) {
          for (Namespace namespace : namespaces) {
            if (namespace.isPossibleName(namespaceText)) {
              page.setNamespace(namespace.getId());
            }
          }
        }
      }
      if (page.getNamespace() == null) {
        page.setNamespace(Namespace.MAIN);
      }
    }

    // Manage comments
    Configuration config = Configuration.getConfiguration();
    Object comment = config.getPojo(
        wikipedia, Configuration.POJO_PAGE_COMMENTS, page.getTitle(), PageComment.class);
    if (comment instanceof PageComment) {
      page.setComment((PageComment) comment);
    }

    return page;
  }

}
