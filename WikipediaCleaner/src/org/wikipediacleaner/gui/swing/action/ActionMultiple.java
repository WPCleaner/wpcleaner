/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2016  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.AbstractAction;


/**
 * Manage combined actions
 */
public class ActionMultiple extends AbstractAction {

  /** Serialization */
  private static final long serialVersionUID = -1243715047003329003L;

  /** Combined actions */
  private final List<AbstractAction> actions;

  /**
   * Constructor.
   */
  public ActionMultiple() {
    actions = new ArrayList<>();
  }

  /**
   * @param action Action to be added.
   */
  public void addAction(AbstractAction action) {
    if (action != null) {
      actions.add(action);
    }
  }

  /**
   * @param e
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    for (AbstractAction action : actions) {
      action.actionPerformed(e);
    }
  }

}
