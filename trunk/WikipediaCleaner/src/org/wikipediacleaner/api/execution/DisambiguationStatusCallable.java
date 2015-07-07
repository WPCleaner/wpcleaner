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
 * A Callable implementation for retrieving Disambiguation Status.
 */
public class DisambiguationStatusCallable extends MediaWikiCallable<List<Page>> {

  private final List<Page> pages;

  private final boolean forceApiCall;

  /**
   * @param wikipedia Wikipedia.
   * @param listener Listener of MediaWiki events.
   * @param api MediaWiki API.
   * @param pages List of pages.
   * @param forceApiCall True if API call should be forced even if the list of disambiguation pages is loaded.
   */
  public DisambiguationStatusCallable(
      EnumWikipedia wikipedia, MediaWikiListener listener, API api,
      List<Page> pages, boolean forceApiCall) {
    super(wikipedia, listener, api);
    this.pages = pages;
    this.forceApiCall = forceApiCall;
  }

  /* (non-Javadoc)
   * @see java.util.concurrent.Callable#call()
   */
  @Override
  public List<Page> call() throws APIException {
    setText(GT._("Retrieving disambiguation information"));
    api.initializeDisambiguationStatus(getWikipedia(), pages, forceApiCall);
    return pages;
  }

}
