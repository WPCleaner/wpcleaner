/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
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
  @Override
  public List<Page> call() throws APIException {
    setText(GT._T("Retrieving page usage") + " - " + page.getTitle());
    api.retrieveEmbeddedIn(getWikipedia(), page, namespaces, limit);
    return page.getRelatedPages(Page.RelatedPages.EMBEDDED_IN);
  }

}
