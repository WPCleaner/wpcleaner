/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.awt.Component;
import java.awt.event.ActionEvent;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.text.TextAction;


/**
 * An Action implementation that can be created without knowing the MWPane component it will act upon.
 * It must have a way of getting it's target to act upon.
 * This class provides support to try and find a MWPane component to operate on.
 * The preferred way of getting the component to act upon is through the ActionEvent that is received.
 * If the object returned by getSource can be narrowed to a MWPane component, it will be used.
 * 
 */
public abstract class MWPaneAction extends TextAction {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = -312026852059614157L;

  /**
   * @param name Name of the action.
   */
  public MWPaneAction(String name) {
    super(name);
  }

  /**
   * Find the MWPane component to act upon.
   * 
   * @param e ActionEvent received when the event is fired.
   * @return MWPane component to act upon.
   */
  protected final MWPane getMWPane(ActionEvent e) {
    if (e != null) {
      Object o = e.getSource();
      if (o instanceof Component) {
        return getMWPane((Component) o);
      }
    }
    return null;
  }

  /**
   * Find the MWPane component to act upon.
   * 
   * @param component Component.
   * @return MWPane component to act upon.
   */
  protected final MWPane getMWPane(Component component) {
    if (component == null) {
      return null;
    }
    if (component instanceof MWPane) {
      return (MWPane) component;
    }
    if (component instanceof JMenuItem) {
      JMenuItem menuItem = (JMenuItem) component;
      return getMWPane(menuItem.getParent());
    }
    if (component instanceof JPopupMenu) {
      JPopupMenu popup = (JPopupMenu) component;
      return getMWPane(popup.getInvoker());
    }
    return null;
  }
}
