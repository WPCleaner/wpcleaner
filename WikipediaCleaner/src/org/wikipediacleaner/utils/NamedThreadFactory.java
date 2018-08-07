/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.utils;

import java.text.MessageFormat;
import java.util.concurrent.ThreadFactory;


/**
 * A thread factory to allow naming threads properly.
 */
public class NamedThreadFactory implements ThreadFactory {

  /** Parent thread factory, which actually creates the threads */
  private ThreadFactory parent;

  /** Base name for the threads (include a {0} for the thread number, see {@link java.text.MessageFormat}) */
  private String basename;

  /** Counter for the number of threads created s*/
  private int counter;

  /**
   * Constructor.
   * 
   * @param parent Parent thread factory, which actually creates the threads.
   * @param basename Base name for the threads (include a {0} for the thread number, see {@link java.text.MessageFormat}).
   */
  public NamedThreadFactory(
      ThreadFactory parent,
      String basename) {
    this.parent = parent;
    this.basename = basename;
  }

  /**
   * @param r
   * @return
   * @see java.util.concurrent.ThreadFactory#newThread(java.lang.Runnable)
   */
  @Override
  public Thread newThread(Runnable r) {
    Thread thread = parent.newThread(r);
    Integer numThread = null;
    synchronized (basename) {
      counter++;
      numThread = Integer.valueOf(counter);
    }
    thread.setName(MessageFormat.format(basename, numThread));
    return thread;
  }

}
