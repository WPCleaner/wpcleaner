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

  public void setText(String text) {
    if (listener != null) {
      listener.setText(text);
    }
  }

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
