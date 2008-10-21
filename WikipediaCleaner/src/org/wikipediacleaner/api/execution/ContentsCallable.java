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

import org.wikipediacleaner.api.MediaWikiListener;
import org.wikipediacleaner.api.base.API;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.i18n.GT;


/**
 * A Callable implementation for retrieving Contents.
 */
public class ContentsCallable extends MediaWikiCallable<Page> {

  private final Page page;
  private final Page returnPage;

  /**
   * @param listener Listener of MediaWiki events.
   * @param api MediaWiki API.
   * @param page Page.
   * @param returnPage Page to return at the end of the processing.
   */
  public ContentsCallable(MediaWikiListener listener, API api, Page page, Page returnPage) {
    super(listener, api);
    this.page = page;
    this.returnPage = returnPage;
  }

  /* (non-Javadoc)
   * @see java.util.concurrent.Callable#call()
   */
  public Page call() throws APIException {
    setText(GT._("Retrieving contents") + " - " + page.getTitle());
    api.retrieveContents(page);
    return returnPage;
  }

}
