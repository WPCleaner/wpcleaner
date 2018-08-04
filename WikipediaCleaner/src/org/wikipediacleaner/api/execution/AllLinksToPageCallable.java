/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
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
public class AllLinksToPageCallable extends MediaWikiCallable<Page> {

  private final Page page;

  /**
   * @param wikipedia Wikipedia.
   * @param listener Listener of MediaWiki events.
   * @param api MediaWiki API.
   * @param page Page.
   */
  public AllLinksToPageCallable(
      EnumWikipedia wikipedia, MediaWikiListener listener, API api,
      Page page) {
    super(wikipedia, listener, api);
    this.page = page;
  }

  /* (non-Javadoc)
   * @see java.util.concurrent.Callable#call()
   */
  @Override
  public Page call() throws APIException {
    setText(GT._T("Retrieving all links to page") + " - " + page.getTitle());
    api.retrieveLinksHere(getWikipedia(), page, true);
    return page;
  }

}
