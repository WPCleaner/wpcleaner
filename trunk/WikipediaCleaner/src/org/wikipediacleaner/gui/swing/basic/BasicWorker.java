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

package org.wikipediacleaner.gui.swing.basic;

import org.wikipediacleaner.api.MediaWikiListener;
import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * 
 */
public class BasicWorker extends SwingWorker implements MediaWikiListener {

  private final EnumWikipedia wikipedia;
  private final BasicWindow window;
  private BasicWorkerListener listener;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   */
  public BasicWorker(EnumWikipedia wikipedia, BasicWindow window) {
    super();
    this.wikipedia = wikipedia;
    this.window = window;
  }

  /**
   * Modify the displayed message.
   * 
   * @param message Message to display.
   */
  public void setText(String message) {
    if ((window != null) && (window.getGlassPane() != null)) {
      window.getGlassPane().setText(message);
    }
  }

  /**
   * @param listener Listener.
   */
  public void setListener(BasicWorkerListener listener) {
    this.listener = listener;
  }

  /**
   * @return Flag indicating if the Swing Worker should stop.
   */
  public boolean shouldStop() {
    return !shouldContinue();
  }

  /**
   * @return Flag indicating if the Swing Worker should continue.
   */
  public boolean shouldContinue() {
    if ((window == null) ||
        (window.getParentComponent() == null) ||
        (window.getParentComponent().isDisplayable() == false)) {
      return false;
    }
    return true;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.SwingWorker#start()
   */
  @Override
  public void start() {
    if ((window != null) && (window.getGlassPane() != null)) {
      window.getGlassPane().start();
    }
    if (listener != null) {
      listener.beforeStart(this);
    }
    super.start();
    if (listener != null) {
      listener.afterStart(this);
    }
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.SwingWorker#construct()
   */
  @Override
  public Object construct() {
    return null;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.SwingWorker#finished()
   */
  @Override
  public void finished() {
    if (listener != null) {
      listener.beforeFinished(this);
    }
    super.finished();
    Object result = get();
    boolean ok = true;
    if (result instanceof Throwable) {
      ok = false;
    }
    if (window != null) {
      if (result instanceof Throwable) {
        window.displayError((Throwable) result);
      }
      window.updateComponentState();
    }
    if (listener != null) {
      listener.afterFinished(this, ok);
    }
    if ((window != null) && (window.getGlassPane() != null)) {
      window.getGlassPane().stop();
    }
  }

  /**
   * @return Associated window.
   */
  public BasicWindow getWindow() {
    return window;
  }

  /**
   * @return Wikipedia.
   */
  public EnumWikipedia getWikipedia() {
    return wikipedia;
  }
}
