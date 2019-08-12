/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.basic;

import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.action.ActionDispose;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;


/**
 * A basic window displaying a JPanl and a close button. 
 */
public abstract class PanelWindow extends BasicWindow {

  /** Title of the window */
  private String title;

  /** Panel to use inside the window */
  private JPanel panel;

  /** Close button */
  private JButton buttonClose;

  /**
   * Create and display a PanelWindow.
   * 
   * @param name Window name.
   * @param wiki Wiki.
   * @param windowClass Class of the window.
   * @param title Title of the window.
   * @param panel JPanel to use inside the window.
   */
  protected static void createPanelWindow(
      String name,
      EnumWikipedia wiki,
      Class<? extends PanelWindow> windowClass,
      final String title,
      final JPanel panel) {
    createWindow(
        name, wiki,
        WindowConstants.DISPOSE_ON_CLOSE,
        windowClass,
        new DefaultBasicWindowListener() {
          
          @Override
          public void initializeWindow(BasicWindow window) {
            PanelWindow.initializeWindow(window, title, panel);
          }
        });
  }

  /**
   * Initialize window.
   * 
   * @param window Window.
   * @param title Title of the window.
   * @param panel JPanel to use inside the window.
   */
  static void initializeWindow(
      BasicWindow window,
      String title,
      JPanel panel) {
    if (window instanceof PanelWindow) {
      PanelWindow panelWindow = (PanelWindow) window;
      panelWindow.setPanel(panel);
      panelWindow.setTitle(title);
    }
  }

  /**
   * @param title Title of the window.
   */
  protected void setTitle(String title) {
    this.title = title;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return title;
  }

  /**
   * @param panel JPanel to use inside the window.
   */
  protected void setPanel(JPanel panel) {
    this.panel = panel;
  }

  /**
   * @return Window components.
   */
  @Override
  protected Component createComponents() {
    JPanel bigPanel = new JPanel(new GridBagLayout());

    // Initialize constraints
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(2, 2, 2, 2);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Panel
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weightx = 1;
    constraints.weighty = 1;
    bigPanel.add(panel, constraints);
    constraints.gridy++;

    // Commands
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weighty = 0;
    bigPanel.add(createCommandComponents(), constraints);
    constraints.gridy++;

    return bigPanel;
  }

  /**
   * @return Command components.
   */
  private Component createCommandComponents() {
    JPanel commandPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
    //panel.setBorder(BorderFactory.createEtchedBorder());

    // Close button
    buttonClose = ActionDispose.createButton(getParentComponent(), true, false);
    commandPanel.add(buttonClose);

    return commandPanel;
  }
}
