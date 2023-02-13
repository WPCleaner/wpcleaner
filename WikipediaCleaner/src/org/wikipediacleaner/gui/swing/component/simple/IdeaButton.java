/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2019  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.component.simple;

import java.awt.Component;
import java.awt.event.ActionListener;
import java.beans.EventHandler;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.ConfigurationValueShortcut;

/**
 * An helper class to build a help button.
 */
public class IdeaButton {

  /** Parent component */
  private final Component parentComponent;

  /** Button for ideas */
  private final JButton button;

  /**
   * Constructor.
   * 
   * @param parentComponent Parent component.
   */
  public IdeaButton(Component parentComponent) {
    this.parentComponent = parentComponent;
    
    // Create button
    button = Utilities.createJButton(
        GT._T("Idea? Bug?"),
        ConfigurationValueShortcut.BUG_REPORT);
    button.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionIdea"));
  }

  /**
   * @return Button for ideas.
   */
  public JComponent getButton() {
    return button;
  }

  /**
   * Action called when Idea button is pressed.
   */
  public void actionIdea() {
    JPopupMenu menu = new JPopupMenu();
    JMenuItem reportBug = new JMenuItem(GT._T("Report bug"));
    reportBug.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionReportBug"));
    menu.add(reportBug);
    JMenuItem featureRequest = new JMenuItem(GT._T("Request a new feature"));
    featureRequest.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionRequestNewFeature"));
    menu.add(featureRequest);
    menu.show(button, 0, button.getHeight());
  }

  /**
   * Action called when Report Bug button is pressed.
   */
  public void actionReportBug() {
    StringBuilder url = new StringBuilder();
    url.append("https://phabricator.wikimedia.org/maniphest/task/edit/form/43/");
    url.append("?projects=wpcleaner&subscribers=NicoV");
    final String baseUrl = url.toString();
    try {
      StringBuilder description = new StringBuilder();
      description.append("**Steps to replicate the issue** (include links if applicable):\n");
      description.append("\n");
      description.append("*\n");
      description.append("*\n");
      description.append("*\n");
      description.append("\n");
      description.append("**What happens?**:\n");
      description.append("\n");
      description.append("\n");
      description.append("**What should have happened instead?**:\n");
      description.append("\n");
      description.append("\n");
      description.append("**Other information** (browser name/version, screenshots, etc.):\n");
      description.append("\n");
      description.append("\n");
      description.append("**Information provided by WPCleaner**:\n");
      description.append("* Java version: " + System.getProperty("java.version") + "\n");
      description.append("* Java vendor: " + System.getProperty("java.vendor") + "\n");
      description.append("* Operating system: " + System.getProperty("os.name") + "\n");
      url.append("&description=" + URLEncoder.encode(description.toString(), StandardCharsets.UTF_8.name()));
    } catch (UnsupportedEncodingException e) {
      // Do nothing
    }
    Utilities.browseURL(url.toString(), () -> Utilities.displayUrlMessage(
        parentComponent,
        GT._T("You can submit bug reports at the following URL:"),
        baseUrl));
  }

  /**
   * Action called when Request New Feature button is pressed.
   */
  public void actionRequestNewFeature() {
    StringBuilder url = new StringBuilder();
    url.append("https://phabricator.wikimedia.org/maniphest/task/edit/form/102/");
    url.append("?projects=wpcleaner&subscribers=NicoV");
    Utilities.browseURL(url.toString(), () -> Utilities.displayUrlMessage(
        parentComponent,
        GT._T("You can submit feature requests at the following URL:"),
        url.toString()));
  }
}
