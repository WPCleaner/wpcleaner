/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
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
  private Integer timeLimit;
  private long startTime;

  /**
   * @param wikipedia Wikipedia.
   * @param window Window.
   */
  public BasicWorker(EnumWikipedia wikipedia, BasicWindow window) {
    super();
    this.wikipedia = wikipedia;
    this.window = window;
    this.timeLimit = null;
    this.startTime = System.currentTimeMillis();
  }

  /**
   * Modify the displayed message.
   * 
   * @param message Message to display.
   */
  @Override
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
   * @param limit Time limit for execution.
   */
  public void setTimeLimit(Integer limit) {
    this.timeLimit = limit;
    this.startTime = System.currentTimeMillis();
  }

  /**
   * @return Flag indicating if the Swing Worker should stop.
   */
  @Override
  public boolean shouldStop() {
    return !shouldContinue();
  }

  /**
   * @return Flag indicating if the Swing Worker should continue.
   */
  public boolean shouldContinue() {
    if (timeLimit != null) {
      long currentTime = System.currentTimeMillis();
      if ((currentTime - startTime) / 1000 > timeLimit.intValue()) {
        return false;
      }
    }
    if (window == null) {
      return true;
    }
    if ((window.getParentComponent() == null) ||
        !window.getParentComponent().isDisplayable()) {
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
    startTime = System.currentTimeMillis();
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
    try {
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
    } catch (Throwable t) {
      if (window != null) {
        window.displayError(t);
      }
    } finally {
      if ((window != null) && (window.getGlassPane() != null)) {
        window.getGlassPane().stop();
      }
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
