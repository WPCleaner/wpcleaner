/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.gui.swing;

import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.beans.EventHandler;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.WindowConstants;

import org.wikipediacleaner.Version;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;


/**
 * About WikipediaCleaner. 
 */
public class AboutWindow extends BasicWindow {

  private JButton buttonClose;

  /**
   * Create and display an AboutWindow.
   */
  public static void createAboutWindow() {
    createWindow(
        "AboutWindow", null,
        WindowConstants.DISPOSE_ON_CLOSE,
        AboutWindow.class,
        null);
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._("About") + " - " + Version.PROGRAM; 
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
    constraints.weightx = 1;
    constraints.weighty = 0;

    // About
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    panel.add(createAboutComponents(), constraints);
    constraints.gridy++;

    // Commands
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weighty = 0;
    panel.add(createCommandComponents(), constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * @return About components.
   */
  private Component createAboutComponents() {
    JTabbedPane pane = new JTabbedPane();
    pane.addTab(Version.PROGRAM, createWPCleanerTab());
    pane.addTab("Commons Logging", createCommonsLoggingTab());
    pane.addTab("Commons Codec", createCommonsCodecTab());
    pane.addTab("Commons HttpClient", createCommonsHttpClientTab());
    pane.addTab("JDOM", createJDomTab());
    pane.addTab("Saxen", createSaxenTab());
    pane.addTab("Cobra", createCobraTab());
    return pane;
  }

  /**
   * @return Wikipedia Cleaner tab.
   */
  private Component createWPCleanerTab() {
    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    //TODO
    return panel;
  }

  /**
   * @return Apache Commons Logging tab.
   */
  private Component createCommonsLoggingTab() {
    JPanel panel = new JPanel(new GridBagLayout());
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

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

    // Short presentation
    JLabel presentation = new JLabel();
    presentation.setText(
        "<html>" +
        "<b><a href='http://commons.apache.org/logging/'>Apache Commons Logging</a></b> is a component of Commons Apache projet." +
        "<br>" +
        "The Logging package is an ultra-thin bridge between different logging implementations." +
        "<br>" +
        "See http://commons.apache.org/logging/ for more information." +
        "</html>");
    panel.add(presentation, constraints);
    constraints.gridy++;

    //TODO: License

    return panel;
  }

  /**
   * @return Apache Commons Codec tab.
   */
  private Component createCommonsCodecTab() {
    JPanel panel = new JPanel(new GridBagLayout());
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

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

    // Short presentation
    JLabel presentation = new JLabel();
    presentation.setText(
        "<html>" +
        "<b><a href='http://commons.apache.org/codec/'>Apache Commons Codec</a></b> is a component of Commons Apache projet." +
        "<br>" +
        "Commons Codec provides implementations of common encoders and decoders such as Base64, Hex, Phonetic and URLs." +
        "<br>" +
        "See http://commons.apache.org/codec/ for more information." +
        "</html>");
    panel.add(presentation, constraints);
    constraints.gridy++;

    //TODO: License

    return panel;
  }

  /**
   * @return Apache Commons HttpClient tab.
   */
  private Component createCommonsHttpClientTab() {
    JPanel panel = new JPanel(new GridBagLayout());
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

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

    // Short presentation
    JLabel presentation = new JLabel();
    presentation.setText(
        "<html>" +
        "<b><a href='http://jakarta.apache.org/httpcomponents/httpclient-3.x/'>Jakarta Commons HttpClient</a></b> is an Apache projet." +
        "<br>" +
        "See http://jakarta.apache.org/httpcomponents/httpclient-3.x/ for more information." +
        "</html>");
    panel.add(presentation, constraints);
    constraints.gridy++;

    //TODO: License

    return panel;
  }

  /**
   * @return JDOM tab.
   */
  private Component createJDomTab() {
    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    //TODO
    return panel;
  }

  /**
   * @return Saxen tab.
   */
  private Component createSaxenTab() {
    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    //TODO
    return panel;
  }

  /**
   * @return Cobra tab.
   */
  private Component createCobraTab() {
    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    //TODO
    return panel;
  }

  /**
   * @return Login components.
   */
  private Component createCommandComponents() {
    JPanel panel = new JPanel(new FlowLayout(FlowLayout.CENTER));
    //panel.setBorder(BorderFactory.createEtchedBorder());

    // Close button
    buttonClose = Utilities.createJButton(GT._("&Close"));
    buttonClose.addActionListener(EventHandler.create(
        ActionListener.class, this, "dispose"));
    panel.add(buttonClose);

    return panel;
  }
}
