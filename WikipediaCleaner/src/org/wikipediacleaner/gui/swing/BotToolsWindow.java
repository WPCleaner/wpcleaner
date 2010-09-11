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

import java.awt.Color;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.WindowConstants;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;


/**
 * A window for bot tools.
 */
public class BotToolsWindow
  extends BasicWindow
  implements ActionListener {

  private final static String ACTION_AUTOMATIC_FIXING   = "AUTOMATIC FIXING";
  private final static String ACTION_CLOSE              = "CLOSE";
  private final static String ACTION_UPDATE_DAB_WARNING = "UPDATE DAB WARNING";

  private JButton buttonAutomaticFixing;
  private JButton buttonUpdateDabWarning;
  private JButton buttonClose;

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

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._("Bot tools");
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
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 0;
    constraints.weighty = 0;

    // Warning
    String txtWarning =
      GT._("!!! WARNING !!!") + "\n" +
      GT._("Functions available here are considered as bot tools.") + "\n" +
      GT._("They may modify a lot of pages in a short period of time.") + "\n" +
      GT._("On some Wikipedia projects, you may need the bot status for doing this.") + "\n" +
      GT._("Please, check if you need the bot status by reading the rules of Wikipedia.");
    JTextArea lblWarning = new JTextArea(txtWarning);
    lblWarning.setEditable(false);
    lblWarning.setBackground(getParentComponent().getBackground());
    lblWarning.setForeground(Color.RED);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(lblWarning, constraints);
    constraints.gridy++;

    // Tools
    buttonAutomaticFixing = Utilities.createJButton(
        "commons-disambig-colour.png", EnumImageSize.NORMAL,
        GT._("Semi-automatic disambiguation fixing"), true);
    buttonAutomaticFixing.setActionCommand(ACTION_AUTOMATIC_FIXING);
    buttonAutomaticFixing.addActionListener(this);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridx = 0;
    constraints.weighty = 1;
    constraints.weightx = 0;
    panel.add(buttonAutomaticFixing, constraints);
    constraints.gridy++;

    buttonUpdateDabWarning = Utilities.createJButton(
        "commons-disambig-colour.png", EnumImageSize.NORMAL,
        GT._("Update existing disambiguation warning messages"), true);
    buttonUpdateDabWarning.setActionCommand(ACTION_UPDATE_DAB_WARNING);
    buttonUpdateDabWarning.addActionListener(this);
    panel.add(buttonUpdateDabWarning, constraints);
    constraints.gridy++;

    // Buttons
    JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    buttonClose = Utilities.createJButton(GT._("&Close"));
    buttonClose.setActionCommand(ACTION_CLOSE);
    buttonClose.addActionListener(this);
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
    buttonAutomaticFixing.setEnabled(false);
    buttonUpdateDabWarning.setEnabled(false);
  }

  /**
   * Invoked when an action occurs.
   * 
   * @param e Event.
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if (e == null) {
      return;
    }

    if (ACTION_CLOSE.equals(e.getActionCommand())) {
      actionClose();
    } else if (ACTION_AUTOMATIC_FIXING.equals(e.getActionCommand())) {
      actionAutomaticFixing();
    } else if (ACTION_UPDATE_DAB_WARNING.equals(e.getActionCommand())) {
      actionUpdateDabWarning();
    }
  }

  /**
   * Action called when Close button is pressed.
   */
  private void actionClose() {
    dispose();
  }

  /**
   * Action called when Automatic Fixing button is pressed.
   */
  private void actionAutomaticFixing() {
    // TODO
  }

  /**
   * Action called when Update Dab Warning button is pressed.
   */
  private void actionUpdateDabWarning() {
    // TODO
  }
}
