/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.basic;


/**
 * An interface used to listen for BasicWorker events.
 */
public interface BasicWorkerListener {

  /**
   * Called just at the beginning of the start() method in BasicWorker.
   * 
   * @param worker Current worker.
   */
  void beforeStart(BasicWorker worker);

  /**
   * Called just at the end of the start() method in BasicWorker.
   * 
   * @param worker Current worker.
   */
  void afterStart(BasicWorker worker);

  /**
   * Called just at the beginning of the finished() method in BasicWorker.
   * 
   * @param worker Current worker.
   */
  void beforeFinished(BasicWorker worker);
  
  /**
   * Called just at the end of the finished() method in BasicWorker.
   * 
   * @param worker Current worker.
   * @param ok Flag indicating if the worker finished OK.
   */
  void afterFinished(BasicWorker worker, boolean ok);
}
