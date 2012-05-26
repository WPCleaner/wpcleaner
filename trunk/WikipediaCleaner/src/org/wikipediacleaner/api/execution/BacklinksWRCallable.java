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

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.MediaWikiListener;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.i18n.GT;


/**
 * A Callable implementation for retrieving Backlinks with Redirects.
 */
public class BacklinksWRCallable extends MediaWikiCallable<Page> {

  private final Page page;

  /**
   * @param wikipedia Wikipedia.
   * @param listener Listener of MediaWiki events.
   * @param api MediaWiki API.
   * @param page Page.
   */
  public BacklinksWRCallable(
      EnumWikipedia wikipedia, MediaWikiListener listener, API api,
      Page page) {
    super(wikipedia, listener, api);
    this.page = page;
  }

  /* (non-Javadoc)
   * @see java.util.concurrent.Callable#call()
   */
  public Page call() throws APIException {
    setText(GT._("Retrieving page back links") + " - " + page.getTitle());
    api.retrieveBackLinksWithRedirects(getWikipedia(), page);
    return page;
  }

}
