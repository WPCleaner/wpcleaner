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
 * A utility class to run a task of ask for a confirmation.
 */
public class TaskConfirmDialog implements Runnable {

  /** Parent component */
  final Component parent;

  /** Message to display */
  final String message;

  /** Type of options */
  final int optionType;

  /** Result */
  Integer result;

  /**
   * @param parent Parent component.
   * @param message Message.
   * @param optionType Type of option.
   */
  public TaskConfirmDialog(
      Component parent,
      String message,
      int optionType) {
    this.parent = parent;
    this.message = message;
    this.optionType = optionType;
    this.result = null;
  }

  /**
   * @see java.lang.Runnable#run()
   */
  @Override
  public void run() {
    result = JOptionPane.showConfirmDialog(
        parent, message, Version.PROGRAM,
        optionType, JOptionPane.WARNING_MESSAGE);
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
