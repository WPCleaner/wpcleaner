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
import org.wikipediacleaner.i18n.GT;


/**
 * A Callable implementation for expanding templates.
 */
public class ExpandTemplatesCallable extends MediaWikiCallable<String> {

  private final String title;
  private final String text;

  /**
   * @param wikipedia Wikipedia.
   * @param listener Listener of MediaWiki events.
   * @param api MediaWiki API.
   * @param title Page title.
   * @param text Page text.
   */
  public ExpandTemplatesCallable(
      EnumWikipedia wikipedia, MediaWikiListener listener, API api,
      String title, String text) {
    super(wikipedia, listener, api);
    this.title = title;
    this.text = text;
  }

  /* (non-Javadoc)
   * @see java.util.concurrent.Callable#call()
   */
  @Override
  public String call() throws APIException {
    setText(GT._("Expanding templates") + " - " + title);
    return api.expandTemplates(getWikipedia(), title, text);
  }

}
