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
 * A utility class to run a task of displaying a message.
 */
public class TaskMessageDialog implements Runnable {

  /** Parent component */
  final Component parent;

  /** Message to display */
  final String message;

  /** Optional component to give focus to */
  final Component focus;

  /** Type of message */
  final int messageType;

  /**
   * @param parent Parent component.
   * @param message Message.
   * @param focus Component to give focus to.
   * @param messageType Message type.
   */
  public TaskMessageDialog(
      Component parent,
      String message,
      Component focus,
      int messageType) {
    this.parent = parent;
    this.message = message;
    this.focus = focus;
    this.messageType = messageType;
  }

  /**
   * @see java.lang.Runnable#run()
   */
  @Override
  public void run() {
    JOptionPane.showMessageDialog(parent, message, Version.PROGRAM, messageType);
    if (focus != null) {
      focus.requestFocusInWindow();
    }
  }

}
