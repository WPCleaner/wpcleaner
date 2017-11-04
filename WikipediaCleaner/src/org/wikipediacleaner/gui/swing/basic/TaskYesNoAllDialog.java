/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2016  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.basic;

import java.awt.Component;

import javax.swing.JDialog;
import javax.swing.JOptionPane;

import org.wikipediacleaner.Version;
import org.wikipediacleaner.i18n.GT;


/**
 * A utility class to run a task of ask for a yes/yes all/no/no all answer.
 */
public class TaskYesNoAllDialog implements Runnable {

  /** Parent component */
  final Component parent;

  /** Message to display */
  final String message;

  /** Result */
  Integer result;

  /**
   * @param parent Parent component.
   * @param message Message.
   */
  public TaskYesNoAllDialog(
      Component parent,
      String message) {
    this.parent = parent;
    this.message = message;
    this.result = null;
  }

  /**
   * @see java.lang.Runnable#run()
   */
  @Override
  public void run() {
    Object[] options = new Object[] {
        GT._("Yes"),
        GT._("Yes to all"),
        GT._("No"),
        GT._("No to all"),
    }; 
    JOptionPane pane = new JOptionPane(
        message, JOptionPane.WARNING_MESSAGE,
        JOptionPane.YES_NO_OPTION, null, options);
    JDialog dialog = pane.createDialog(parent, Version.PROGRAM);
    dialog.setVisible(true);
    Object selectedValue = pane.getValue();
    if (selectedValue == null) {
      result = JOptionPane.CLOSED_OPTION;
    } else if (options[0].equals(selectedValue)) {
      result = JOptionPane.YES_OPTION;
    } else if (options[1].equals(selectedValue)) {
      result = Utilities.YES_ALL_OPTION;
    } else if (options[2].equals(selectedValue)) {
      result = JOptionPane.NO_OPTION;
    } else if (options[3].equals(selectedValue)) {
      result =  Utilities.NO_ALL_OPTION;
    } else {
      result = JOptionPane.CLOSED_OPTION;
    }
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
