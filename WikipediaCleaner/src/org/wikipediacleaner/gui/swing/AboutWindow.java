/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.ScrollPaneConstants;
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
    return GT._T("About") + " - " + Version.PROGRAM; 
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
    pane.addTab("System", createSystemTab());
    pane.addTab("Commons Codec", createCommonsCodecTab());
    pane.addTab("Commons Compress", createCommonsCompressTab());
    pane.addTab("Commons HttpClient", createCommonsHttpClientTab());
    pane.addTab("Commons IO", createCommonsIOTab());
    pane.addTab("Commons Lang3", createCommonsLang3Tab());
    pane.addTab("Commons Logging", createCommonsLoggingTab());
    pane.addTab("Gettext Commons", createGettextCommonsTab());
    pane.addTab("JDOM", createJDomTab());
    pane.addTab("Jaxen", createJaxenTab());
    pane.addTab("Jackson", createJacksonTab());
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
   * Add a file contents to a panel.
   * 
   * @param panel Panel Panel in which file contents is added.
   * @param constraints Constraints for adding the file contents.
   * @param fileName File name.
   */
  private void addFile(
      JPanel panel, GridBagConstraints constraints,
      String fileName) {
    String contents = loadFile(fileName);
    addPresentation(panel, constraints, contents);
  }

  /**
   * Add a presentation to a panel.
   * 
   * @param panel Panel Panel in which presentation is added.
   * @param constraints Constraints for adding the presentation.
   * @param text Presentation text.
   */
  private void addPresentation(
      JPanel panel, GridBagConstraints constraints,
      String text) {
    if ((text != null) && (!"".equals(text.trim()))) {
      JLabel label = new JLabel(text);
      label.setBorder(BorderFactory.createLineBorder(Color.BLACK));
      panel.add(label, constraints);
      constraints.gridy++;
    }
  }

  /**
   * Initialize GridBagConstraints for a panel.
   * 
   * @return GridBagConstraints.
   */
  private GridBagConstraints initializeGridBagConstraints() {
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
    return constraints;
  }

  /**
   * Create a JScrollPane.
   * 
   * @param component Component inside the JScrollPane.
   * @return JScrollPane.
   */
  private JScrollPane createScrollPane(JComponent component) {
    JScrollPane scrollPane = new JScrollPane(
        component,
        ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
    scrollPane.setPreferredSize(new Dimension(500, 400));
    return scrollPane;
  }

  /**
   * @return Wikipedia Cleaner tab.
   */
  private Component createWPCleanerTab() {
    JPanel panel = new JPanel(new GridBagLayout());
    GridBagConstraints constraints = initializeGridBagConstraints();
    addPresentation(
        panel, constraints,
        "<html>" +
        "<b>WPCleaner</b> is a tool designed to help with various maintenance tasks." +
        "<br>" +
        "See <a href='http://en.wikipedia.org/wiki/Wikipedia:WPCleaner'>Wikipedia:WPCleaner</a> for more information." +
        "</html>");
    addFile(panel, constraints, "LICENSE.txt");
    return createScrollPane(panel);
  }

  /**
   * @return System tab.
   */
  private Component createSystemTab() {
    JPanel panel = new JPanel(new GridBagLayout());
    GridBagConstraints constraints = initializeGridBagConstraints();
    addPresentation(
        panel, constraints,
        "<html>" +
        "<b>Java version</b>: " + System.getProperty("java.version") + "<br>" +
        "<b>Java vendor</b>: " + System.getProperty("java.vendor") + "<br>" +
        "<b>Java home</b>: " + System.getProperty("java.home") + "<br>" +
        "<b>Java VM version</b>: " + System.getProperty("java.vm.version") + "<br>" +
        "<b>Java VM vendor</b>: " + System.getProperty("java.vm.vendor") + "<br>" +
        "<b>Java specification version</b>: " + System.getProperty("java.specification.version") + "<br>" +
        "<b>Java specification vendor</b>: " + System.getProperty("java.specification.vendor") + "<br>" +
        "<b>Operating system name</b>: " + System.getProperty("os.name") + "<br>" +
        "<b>Operating system architecture</b>: " + System.getProperty("os.arch") + "<br>" +
        "<b>Operating system version</b>: " + System.getProperty("os.version") + "<br>" +
        "</html>");
    return createScrollPane(panel);
  }

  /**
   * @return Apache Commons Logging tab.
   */
  private Component createCommonsLoggingTab() {
    JPanel panel = new JPanel(new GridBagLayout());
    GridBagConstraints constraints = initializeGridBagConstraints();
    addPresentation(
        panel, constraints,
        "<html>" +
        "<b>Apache Commons Logging</b> is a component of Commons Apache project." +
        "<br>" +
        "The Logging package is an ultra-thin bridge between different logging implementations." +
        "<br>" +
        "See <a href='http://commons.apache.org/logging/'>http://commons.apache.org/logging/</a> for more information." +
        "</html>");
    addFile(panel, constraints, "NOTICE_commons-logging.txt");
    addFile(panel, constraints, "LICENSE_commons-logging.txt");
    return createScrollPane(panel);
  }

  /**
   * @return Apache Commons Codec tab.
   */
  private Component createCommonsCodecTab() {
    JPanel panel = new JPanel(new GridBagLayout());
    GridBagConstraints constraints = initializeGridBagConstraints();
    addPresentation(
        panel, constraints,
        "<html>" +
        "<b>Apache Commons Codec</b> is a component of Commons Apache project." +
        "<br>" +
        "Commons Codec provides implementations of common encoders and decoders such as Base64, Hex, Phonetic and URLs." +
        "<br>" +
        "See <a href='http://commons.apache.org/codec/'>http://commons.apache.org/codec/</a> for more information." +
        "</html>");
    addFile(panel, constraints, "NOTICE_commons-codec.txt");
    addFile(panel, constraints, "LICENSE_commons-codec.txt");
    return createScrollPane(panel);
  }

  /**
   * @return Apache Commons Compress tab.
   */
  private Component createCommonsCompressTab() {
    JPanel panel = new JPanel(new GridBagLayout());
    GridBagConstraints constraints = initializeGridBagConstraints();
    addPresentation(
        panel, constraints,
        "<html>" +
        "<b>Apache Commons Compress</b> is a component of Commons Apache project." +
        "<br>" +
        "The Apache Commons Compress library defines an API for working with ar, cpio, Unix dump, tar, zip, gzip, XZ, Pack200, bzip2, 7z, arj, lzma, snappy, DEFLATE and Z files." +
        "<br>" +
        "See <a href='http://commons.apache.org/proper/commons-compress/'>http://commons.apache.org/proper/commons-compress/</a> for more information." +
        "</html>");
    addFile(panel, constraints, "NOTICE_commons-compress.txt");
    addFile(panel, constraints, "LICENSE_commons-compress.txt");
    return createScrollPane(panel);
  }

  /**
   * @return Apache Commons HttpClient tab.
   */
  private Component createCommonsHttpClientTab() {
    JPanel panel = new JPanel(new GridBagLayout());
    GridBagConstraints constraints = initializeGridBagConstraints();
    addPresentation(
        panel, constraints,
        "<html>" +
        "<b>Jakarta Commons HttpClient</b> is an Apache project." +
        "<br>" +
        "See <a href='http://jakarta.apache.org/httpcomponents/httpclient-3.x/'>http://jakarta.apache.org/httpcomponents/httpclient-3.x/</a> for more information." +
        "</html>");
    addFile(panel, constraints, "NOTICE_commons-httpclient.txt");
    addFile(panel, constraints, "LICENSE_commons-httpclient.txt");
    return createScrollPane(panel);
  }

  /**
   * @return Apache Commons IO tab.
   */
  private Component createCommonsIOTab() {
    JPanel panel = new JPanel(new GridBagLayout());
    GridBagConstraints constraints = initializeGridBagConstraints();
    addPresentation(
        panel, constraints,
        "<html>" +
        "<b>Commons IO</b> is an Apache project." +
        "<br>" +
        "See <a href='https://commons.apache.org/proper/commons-io/'>https://commons.apache.org/proper/commons-io/</a> for more information." +
        "</html>");
    addFile(panel, constraints, "NOTICE_commons-io.txt");
    addFile(panel, constraints, "LICENSE_commons-io.txt");
    return createScrollPane(panel);
  }

  /**
   * @return Apache Commons Lang3 tab.
   */
  private Component createCommonsLang3Tab() {
    JPanel panel = new JPanel(new GridBagLayout());
    GridBagConstraints constraints = initializeGridBagConstraints();
    addPresentation(
        panel, constraints,
        "<html>" +
        "<b>Commons Lang</b> is an Apache project." +
        "<br>" +
        "See <a href='http://commons.apache.org/proper/commons-lang/'>http://commons.apache.org/proper/commons-lang/</a> for more information." +
        "</html>");
    addFile(panel, constraints, "NOTICE_commons-lang3.txt");
    addFile(panel, constraints, "LICENSE_commons-lang3.txt");
    return createScrollPane(panel);
  }

  /**
   * @return Gettext Commons tab.
   */
  private Component createGettextCommonsTab() {
    JPanel panel = new JPanel(new GridBagLayout());
    GridBagConstraints constraints = initializeGridBagConstraints();
    addPresentation(
        panel, constraints,
        "<html>" +
        "<b>Gettext Commons</b> provides Java classes for internationalization (i18n)." +
        "<br>" +
        "See <a href='https://www.gnu.org/software/gettext/'>https://www.gnu.org/software/gettext/</a> for more information." +
        "</html>");
    addFile(panel, constraints,"LICENSE_gettext-commons.txt");
    return createScrollPane(panel);
  }

  /**
   * @return JDOM tab.
   */
  private Component createJDomTab() {
    JPanel panel = new JPanel(new GridBagLayout());
    GridBagConstraints constraints = initializeGridBagConstraints();
    addPresentation(
        panel, constraints,
        "<html>" +
        "<b>JDOM</b> is a Java-based solution for accessing, manipulating, and outputting XML data from Java code." +
        "<br>" +
        "See <a href='http://www.jdom.org/index.html'>http://www.jdom.org/index.html</a> for more information." +
        "</html");
        addFile(panel, constraints, "LICENSE_jdom.txt");
        return createScrollPane(panel);
 }

  /**
   * @return Jaxen tab.
   */
  private Component createJaxenTab() {
    JPanel panel = new JPanel(new GridBagLayout());
    GridBagConstraints constraints = initializeGridBagConstraints();
    addPresentation(
        panel, constraints,
        "<html>" +
        "<b>Jaxen</b> is an open source XPath library written in Java." +
        "<br>" +
        "See <a href='http://jaxen.org/'>http://jaxen.org/</a> for more information." +
        "</html>");
        addFile(panel, constraints,"LICENSE_jaxen.txt");
        return createScrollPane(panel);
 }

  /**
   * @return Jackson tab.
   */
  private Component createJacksonTab() {
    JPanel panel = new JPanel(new GridBagLayout());
    GridBagConstraints constraints = initializeGridBagConstraints();
    addPresentation(
        panel, constraints,
        "<html>" +
        "<b>Jackson</b> is a multi-purpose Java library for processing JSON data format." +
        "<br>" +
        "See <a href='http://wiki.fasterxml.com/JacksonHome'>http://wiki.fasterxml.com/JacksonHome</a> for more information." +
        "</html>");
        addFile(panel, constraints, "LICENSE_jackson.txt");
        return createScrollPane(panel);
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
