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
 * A Callable implementation for retrieving Links with Redirects.
 */
public class LinksWRCallable extends MediaWikiCallable<Page> {

  private final Page page;
  private final Integer namespace;
  private final List<Page> knownPages;
  private final boolean disambigNeeded;

  /**
   * @param wikipedia Wikipedia.
   * @param listener Listener of MediaWiki events.
   * @param api MediaWiki API.
   * @param page Page.
   * @param namespace If set, retrieve only links in this namespace.
   * @param knownPages Already known pages.
   * @param disambigNeeded True if disambiguation information is needed.
   */
  public LinksWRCallable(
      EnumWikipedia wikipedia, MediaWikiListener listener, API api,
      Page page, Integer namespace, List<Page> knownPages,
      boolean disambigNeeded) {
    super(wikipedia, listener, api);
    this.page = page;
    this.namespace = namespace;
    this.knownPages = knownPages;
    this.disambigNeeded = disambigNeeded;
  }

  /* (non-Javadoc)
   * @see java.util.concurrent.Callable#call()
   */
  @Override
  public Page call() throws APIException {
    setText(GT._("Retrieving page links") + " - " + page.getTitle());
    api.retrieveLinks(getWikipedia(), page, namespace, knownPages, true, disambigNeeded);
    return page;
  }

}
