/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.bot;

import java.awt.Color;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.WindowConstants;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.action.ActionDispose;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.i18n.GT;


/**
 * A window for bot tools.
 */
public class BotToolsWindow
  extends BasicWindow {

  public final static Integer WINDOW_VERSION = Integer.valueOf(5);

  public final List<BotToolsPanel> panels = new ArrayList<>();

  /**
   * Create and display a BotToolsWindow.
   * 
   * @param wikipedia Wikipedia.
   */
  public static void createBotToolsWindow(
      final EnumWikipedia wikipedia) {
    createWindow(
        "BotToolsWindow",
        wikipedia,
        WindowConstants.DISPOSE_ON_CLOSE,
        BotToolsWindow.class,
        null);
  }

  /**
   * @return Window title.
   */
  @Override
  public String getTitle() {
    return GT._T("Bot tools");
  }

  /**
   * @return Window components.
   */
  @Override
  protected Component createComponents() {
    JPanel panel = new JPanel(new GridBagLayout());

    // Initialize constraints
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Warning
    String txtWarning =
      GT._T("!!! WARNING !!!") + "\n" +
      GT._T("Functions available here are considered as bot tools.") + "\n" +
      GT._T("They may modify a lot of pages in a short period of time.") + "\n" +
      GT._T("On some Wikipedia projects, you may need the bot status for doing this.") + "\n" +
      GT._T("Please, check if you need the bot status by reading the rules of Wikipedia.");
    JTextArea lblWarning = new JTextArea(txtWarning);
    lblWarning.setEditable(false);
    lblWarning.setBackground(getParentComponent().getBackground());
    lblWarning.setForeground(Color.RED);
    panel.add(lblWarning, constraints);
    constraints.gridy++;

    // Tabs
    JTabbedPane pane = new JTabbedPane();
    GeneralToolsPanel generalTools = new GeneralToolsPanel(this);
    pane.addTab(GT._T("General"), generalTools);
    panels.add(generalTools);
    CWToolsPanel cwTools = new CWToolsPanel(this);
    pane.addTab(GT._T("Check Wiki"), cwTools);
    panels.add(cwTools);
    constraints.weighty = 1;
    panel.add(pane, constraints);
    constraints.gridy++;

    // Buttons
    JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    JButton buttonClose = ActionDispose.createButton(getParentComponent(), true, false);
    buttonPanel.add(buttonClose);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(buttonPanel, constraints);
    constraints.gridy++;

    updateComponentState();
    return panel;
  }

  /**
   * Update components states.
   */
  @Override
  protected void updateComponentState() {
    super.updateComponentState();
    for (BotToolsPanel panel : panels) {
      panel.updateComponentState();
    }
  }
}
