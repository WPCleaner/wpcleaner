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

import java.util.Collections;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.MediaWikiListener;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.i18n.GT;


/**
 * A Callable implementation for retrieving Contents.
 */
public class ContentsCallable extends MediaWikiCallable<Page> {

  private final Page page;
  private final Page returnPage;
  private final boolean usePageId;
  private final boolean withRedirects;
  private final Integer section;

  /**
   * @param wikipedia Wikipedia.
   * @param listener Listener of MediaWiki events.
   * @param api MediaWiki API.
   * @param page Page.
   * @param returnPage Page to return at the end of the processing.
   * @param usePageId True if page identifiers should be used.
   * @param withRedirects Flag indicating if redirects information should be retrieved.
   * @param section (Optional) Section of the page.
   */
  public ContentsCallable(
      EnumWikipedia wikipedia, MediaWikiListener listener, API api,
      Page page, Page returnPage, boolean usePageId,
      boolean withRedirects, Integer section) {
    super(wikipedia, listener, api);
    this.page = page;
    this.returnPage = returnPage;
    this.usePageId = usePageId;
    this.withRedirects = withRedirects;
    this.section = section;
  }

  /* (non-Javadoc)
   * @see java.util.concurrent.Callable#call()
   */
  public Page call() throws APIException {
    setText(GT._("Retrieving contents") + " - " + page.getTitle());
    if (section == null) {
      api.retrieveContents(
          getWikipedia(),
          Collections.singletonList(page), 
          usePageId, withRedirects);
      if (withRedirects &&
          page.isRedirect() &&
          (page.getRedirects().size() > 0)) {
        api.retrieveContents(getWikipedia(), page.getRedirects(), false, false);
      }
    } else {
      api.retrieveSectionContents(getWikipedia(), page, section.intValue());
    }
    return returnPage;
  }

}
