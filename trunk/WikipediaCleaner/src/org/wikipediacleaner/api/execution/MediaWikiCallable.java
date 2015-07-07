/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.execution;

import java.util.concurrent.Callable;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.MediaWikiListener;
import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * A base implementation of Callable for MediaWiki. 
 */
public abstract class MediaWikiCallable<T> implements Callable<T>, MediaWikiListener {

  private final EnumWikipedia wikipedia;
  protected final API api;
  private final MediaWikiListener listener;

  /**
   * @param wikipedia Wikipedia.
   * @param listener Listener.
   * @param api API.
   */
  public MediaWikiCallable(EnumWikipedia wikipedia, MediaWikiListener listener, API api) {
    this.wikipedia = wikipedia;
    this.listener = listener;
    this.api = api;
  }

  @Override
  public void setText(String text) {
    if (listener != null) {
      listener.setText(text);
    }
  }

  @Override
  public boolean shouldStop() {
    if (listener != null) {
      return listener.shouldStop();
    }
    return false;
  }

  protected EnumWikipedia getWikipedia() {
    return wikipedia;
  }
}
