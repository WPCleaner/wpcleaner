/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2016  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.basic;

import java.awt.Component;

import javax.swing.JOptionPane;

import org.wikipediacleaner.Version;


/**
 * A utility class to run a task of ask for an option.
 */
public class TaskOptionDialog implements Runnable {

  /** Parent component */
  final Component parent;

  /** Message to display */
  final String message;

  /** Array of possible values */
  final Object[] possibleValues;

  /** Result */
  Integer result;

  /**
   * @param parent Parent component.
   * @param message Message.
   * @param possibleValues Array of possible values.
   */
  public TaskOptionDialog(
      Component parent,
      String message,
      Object[] possibleValues) {
    this.parent = parent;
    this.message = message;
    this.possibleValues = possibleValues;
    this.result = null;
  }

  /**
   * @see java.lang.Runnable#run()
   */
  @Override
  public void run() {
    result = JOptionPane.showOptionDialog(
        parent, message, Version.PROGRAM,
        JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE,
        null, possibleValues, null);
  }

  /**
   * @return Result.
   */
  public int getResult() {
    if (result != null) {
      return result.intValue();
    }
    return JOptionPane.CLOSED_OPTION;
  }
}
