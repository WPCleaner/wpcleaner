/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.Icon;
import javax.swing.InputMap;
import javax.swing.JTabbedPane;
import javax.swing.KeyStroke;


/**
 * A JTabbedPane with icons to close panes.
 */
public class JCloseableTabbedPane
  extends JTabbedPane implements KeyListener {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = 1L;

  /**
   * Constructor.
   */
  public JCloseableTabbedPane() {
    this(TOP, WRAP_TAB_LAYOUT);
  }

  /**
   * Constructor.
   * 
   * @param tabPlacement the placement for the tabs relative to the content
   */
  public JCloseableTabbedPane(int tabPlacement) {
    this(tabPlacement, WRAP_TAB_LAYOUT);
  }

  /**
   * Constructor.
   * 
   * @param tabPlacement the placement for the tabs relative to the content
   * @param tabLayoutPolicy the policy for laying out tabs when all tabs will not fit on one run
   */
  public JCloseableTabbedPane(int tabPlacement, int tabLayoutPolicy) {
    super(tabPlacement, tabLayoutPolicy);
    addKeyListener(this);
    ActionMap actionMap = getActionMap();
    InputMap inputMap = getInputMap(WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
    KeyStroke keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_M, InputEvent.CTRL_DOWN_MASK);
    inputMap.put(keyStroke, "close-current-tab");
    actionMap.put("close-current-tab", new AbstractAction() {
      
      /** Serialization */
      private static final long serialVersionUID = -1886432591524366546L;

      /**
       * @param e Event.
       * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
       */
      @Override
      public void actionPerformed(ActionEvent e) {
        remove(getSelectedIndex());
      }
    });
  }

  /**
     * @param title the title to be displayed in this tab
     * @param icon the icon to be displayed in this tab
     * @param component The component to be displayed when this tab is clicked. 
     * @param tip the tooltip to be displayed for this tab
     * @param index the position to insert this new tab 
   * @see javax.swing.JTabbedPane#insertTab(java.lang.String, javax.swing.Icon, java.awt.Component, java.lang.String, int)
   */
  @Override
  public void insertTab(
      String title, Icon icon, Component component,
      String tip, int index) {
    super.insertTab(title, icon, component, tip, index);
    try {
      Method method = this.getClass().getMethod(
          "setTabComponentAt",
          new Class[] { int.class, Component.class });
      method.invoke(this, index, new CloseTabComponent(title, this));
      component.addKeyListener(this);
      /*if (component instanceof Container) {
        Container container = (Container) component;
        for (Component child : container.getComponents()) {
          child.addKeyListener(this);
        }
      }*/
    } catch (SecurityException e) {
      // Nothing
    } catch (NoSuchMethodException e) {
      // Nothing
    } catch (IllegalArgumentException e) {
      // Nothing
    } catch (IllegalAccessException e) {
      // Nothing
    } catch (InvocationTargetException e) {
      // Nothing
    }
    setIconAt(index, new JTabbedPaneCloseIcon(this, component));
  }

  /**
   * @param e Event.
   * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
   */
  @Override
  public void keyTyped(KeyEvent e) {
    // Nothing to do
  }

  /**
   * @param e Event.
   * @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed(KeyEvent e) {
    if ((e.getKeyCode() == KeyEvent.VK_W) &&
        (e.isControlDown())) {
      remove(getSelectedIndex());
    }
  }

  /**
   * @param e Event.
   * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
   */
  @Override
  public void keyReleased(KeyEvent e) {
    // Nothing to do
  }

}
