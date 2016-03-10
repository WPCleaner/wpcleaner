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
 * A utility class to run a task of ask for a value.
 */
public class TaskInputDialog implements Runnable {

  /** Parent component */
  final Component parent;

  /** Message to display */
  final String message;

  /** Array of possible values */
  final Object[] possibleValues;

  /** Default value */
  final Object defaultValue;

  /** Result */
  Object result;

  /**
   * @param parent Parent component.
   * @param message Message.
   * @param possibleValues Array of possible values.
   * @param defaultValue Default value.
   */
  public TaskInputDialog(
      Component parent,
      String message,
      Object[] possibleValues,
      Object defaultValue) {
    this.parent = parent;
    this.message = message;
    this.possibleValues = possibleValues;
    this.defaultValue = defaultValue;
    this.result = null;
  }

  /**
   * @see java.lang.Runnable#run()
   */
  @Override
  public void run() {
    result = JOptionPane.showInputDialog(
        parent, message, Version.PROGRAM,
        JOptionPane.QUESTION_MESSAGE, null,
        possibleValues, defaultValue);
  }

  /**
   * @return Result.
   */
  public Object getResult() {
    return result;
  }
}
