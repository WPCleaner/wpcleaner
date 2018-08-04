/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.action.ActionDispose;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.HTMLPane;
import org.wikipediacleaner.i18n.GT;


/**
 * A window to show information.
 */
public class InformationWindow
  extends BasicWindow {

  String title;
  String information;
  boolean html;
  JLabel lblTitle;
  JTextPane textPane;
  HTMLPane textInformation;
  private JButton buttonClose;

  /**
   * Create and display an InformationWindow.
   * 
   * @param title Title.
   * @param information Information.
   * @param html True if information is in HTML format.
   * @param wikipedia Wikipedia.
   */
  public static void createInformationWindow(
      final String title,
      final String information,
      final boolean html,
      final EnumWikipedia wikipedia) {
    createWindow(
        "InformationWindow",
        wikipedia,
        WindowConstants.DISPOSE_ON_CLOSE,
        InformationWindow.class,
        new DefaultBasicWindowListener() {
          @Override
          public void initializeWindow(BasicWindow window) {
            if (window instanceof InformationWindow) {
              InformationWindow info = (InformationWindow) window;
              info.title = title;
              info.information = information;
              info.html = html;
            }
          }
          @Override
          public void displayWindow(BasicWindow window) {
            if (window instanceof InformationWindow) {
              InformationWindow info = (InformationWindow) window;
              info.updateInformation();
            }
          }
          
        });
  }

  /**
   * Update information.
   */
  void updateInformation() {
    lblTitle.setText(title);
    if (html) {
      textInformation.setText(information);
    } else {
      textPane.setText(information);
    }
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._T("Information");
  }

  /**
   * @return Window components.
   */
  @Override
  protected Component createComponents() {
    JPanel panel = new JPanel(new GridBagLayout());

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
    constraints.weightx = 0;
    constraints.weighty = 0;

    // Title
    lblTitle = Utilities.createJLabel(title);
    lblTitle.setHorizontalAlignment(SwingConstants.LEADING);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(lblTitle, constraints);
    constraints.gridy++;

    // Information
    Component component = null;
    if (html) {
      textInformation = HTMLPane.createHTMLPane(null);
      component = textInformation;
    } else {
      textPane = new JTextPane();
      textPane.setEditable(false);
      component = textPane;
    }
    JScrollPane scrollPane = new JScrollPane(component);
    scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
    scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    component = scrollPane;
    component.setPreferredSize(new Dimension(500, 500));
    component.setMinimumSize(new Dimension(100, 100));
    lblTitle.setLabelFor(component);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridx = 0;
    constraints.weighty = 1;
    constraints.weightx = 1;
    panel.add(component, constraints);
    constraints.gridy++;

    // Buttons
    JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    buttonClose = ActionDispose.createButton(getParentComponent(), true, false);
    buttonPanel.add(buttonClose);
    constraints.fill = GridBagConstraints.NONE;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(buttonPanel, constraints);
    constraints.gridy++;

    return panel;
  }
}
