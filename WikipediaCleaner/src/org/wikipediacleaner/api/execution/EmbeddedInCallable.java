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

package org.wikipediacleaner.api.execution;

import java.util.List;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.MediaWikiListener;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.i18n.GT;


/**
 * A Callable implementation for retrieving Embedded In pages.
 */
public class EmbeddedInCallable extends MediaWikiCallable<List<Page>> {

  private final Page page;

  private final List<Integer> namespaces;

  private final boolean limit;

  /**
   * @param wikipedia Wikipedia.
   * @param listener Listener of MediaWiki events.
   * @param api MediaWiki API.
   * @param page Page.
   * @param namespaces List of name spaces to look into.
   * @param limit Flag indicating if the number of results should be limited.
   */
  public EmbeddedInCallable(
      EnumWikipedia wikipedia, MediaWikiListener listener, API api,
      Page page, List<Integer> namespaces, boolean limit) {
    super(wikipedia, listener, api);
    this.page = page;
    this.namespaces = namespaces;
    this.limit = limit;
  }

  /* (non-Javadoc)
   * @see java.util.concurrent.Callable#call()
   */
  public List<Page> call() throws APIException {
    setText(GT._("Retrieving page usage") + " - " + page.getTitle());
    api.retrieveEmbeddedIn(getWikipedia(), page, namespaces, limit);
    return page.getRelatedPages(Page.RelatedPages.EMBEDDED_IN);
  }

}
