/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.awt.Component;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javax.swing.Icon;
import javax.swing.JTabbedPane;


/**
 * A JTabbedPane with closeable panes.
 */
public class JCloseableTabbedPane extends JTabbedPane {

  /**
   * Serialisation. 
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
    addKeyListener(new KeyAdapter() {

      /* (non-Javadoc)
       * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
       */
      @Override
      public void keyPressed(KeyEvent e) {
        if ((e.getKeyCode() == KeyEvent.VK_W) &&
            (e.isControlDown())) {
          remove(getSelectedIndex());
        }
      }
      
    });
  }

  /* (non-Javadoc)
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

}
