/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing;

import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.WindowConstants;

import org.wikipediacleaner.Version;
import org.wikipediacleaner.gui.swing.action.ActionDispose;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
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
    pane.addTab("Gettext Commons", createGettextCommonsTab());
    pane.addTab("JDOM", createJDomTab());
    pane.addTab("Jaxen", createJaxenTab());
    pane.addTab("Cobra", createCobraTab());
    return pane;
  }

  /**
   * @param name File name.
   * @return File content.
   */
  private String loadFile(String name) {
    InputStream stream = AboutWindow.class.getResourceAsStream("/" + name);
    if (stream == null) {
      System.err.println("File not found: " + name);
      return null;
    }
    BufferedReader reader = null;
    try {
      reader = new BufferedReader(new InputStreamReader(stream, "UTF8"));
      StringBuilder sb = new StringBuilder();
      String line = null;
      while ((line = reader.readLine()) != null) {
        if (sb.length() > 0) {
          sb.append("<br/>");
        } else {
          sb.append("<html>");
        }
        sb.append(line);
      }
      sb.append("</html>");
      return sb.toString();
    } catch (IOException e) {
      //
    } finally {
      try {
        stream.close();
      } catch (IOException e) {
        //
      }
    }
    return null;
  }

  /**
   * @return Wikipedia Cleaner tab.
   */
  private Component createWPCleanerTab() {
    JPanel panel = new JPanel();

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
    JLabel label = new JLabel();
    label.setText(
        "<html>" +
        "<b>WPCleaner</b> is a tool designed to help with various maintenance tasks." +
        "<br>" +
        "See <a href='http://en.wikipedia.org/wiki/Wikipedia:WPCleaner'>Wikipedia:WPCleaner</a> for more information." +
        "</html>");
    panel.add(label, constraints);
    constraints.gridy++;

    // License
    String license = loadFile("LICENSE.txt");
    if ((license != null) && (!"".equals(license.trim()))) {
      label = new JLabel(license);
      panel.add(label, constraints);
      constraints.gridy++;
    }

    return panel;
  }

  /**
   * @return Apache Commons Logging tab.
   */
  private Component createCommonsLoggingTab() {
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

    // Short presentation
    JLabel label = new JLabel();
    label.setText(
        "<html>" +
        "<b>Apache Commons Logging</b> is a component of Commons Apache projet." +
        "<br>" +
        "The Logging package is an ultra-thin bridge between different logging implementations." +
        "<br>" +
        "See <a href='http://commons.apache.org/logging/'>http://commons.apache.org/logging/</a> for more information." +
        "</html>");
    panel.add(label, constraints);
    constraints.gridy++;

    // Notice
    String notice = loadFile("NOTICE_commons-logging.txt");
    if ((notice != null) && (!"".equals(notice.trim()))) {
      label = new JLabel(notice);
      panel.add(label, constraints);
      constraints.gridy++;
    }

    // License
    String license = loadFile("LICENSE_commons-logging.txt");
    if ((license != null) && (!"".equals(license.trim()))) {
      label = new JLabel(license);
      panel.add(label, constraints);
      constraints.gridy++;
    }

    return panel;
  }

  /**
   * @return Apache Commons Codec tab.
   */
  private Component createCommonsCodecTab() {
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

    // Short presentation
    JLabel presentation = new JLabel();
    presentation.setText(
        "<html>" +
        "<b>Apache Commons Codec</b> is a component of Commons Apache projet." +
        "<br>" +
        "Commons Codec provides implementations of common encoders and decoders such as Base64, Hex, Phonetic and URLs." +
        "<br>" +
        "See <a href='http://commons.apache.org/codec/'>http://commons.apache.org/codec/</a> for more information." +
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
        "<b>Jakarta Commons HttpClient</b> is an Apache projet." +
        "<br>" +
        "See <a href='http://jakarta.apache.org/httpcomponents/httpclient-3.x/'>http://jakarta.apache.org/httpcomponents/httpclient-3.x/</a> for more information." +
        "</html>");
    panel.add(presentation, constraints);
    constraints.gridy++;

    //TODO: License

    return panel;
  }

  /**
   * @return Gettext Commons tab.
   */
  private Component createGettextCommonsTab() {
    JPanel panel = new JPanel(new GridBagLayout());
    //TODO
    return panel;
  }

  /**
   * @return JDOM tab.
   */
  private Component createJDomTab() {
    JPanel panel = new JPanel(new GridBagLayout());
    //TODO
    return panel;
  }

  /**
   * @return Jaxen tab.
   */
  private Component createJaxenTab() {
    JPanel panel = new JPanel(new GridBagLayout());
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
   * @return Command components.
   */
  private Component createCommandComponents() {
    JPanel panel = new JPanel(new FlowLayout(FlowLayout.CENTER));
    //panel.setBorder(BorderFactory.createEtchedBorder());

    // Close button
    buttonClose = ActionDispose.createButton(getParentComponent(), true, false);
    panel.add(buttonClose);

    return panel;
  }
}
