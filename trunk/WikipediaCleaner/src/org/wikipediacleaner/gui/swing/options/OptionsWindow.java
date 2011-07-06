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

package org.wikipediacleaner.gui.swing.options;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.WindowConstants;

import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;


/**
 * Options Window of WikipediaCleaner. 
 */
public class OptionsWindow
  extends BasicWindow
  implements ActionListener {

  private final static String ACTION_APPLY       = "APPLY";
  private final static String ACTION_CANCEL      = "CANCEL";
  private final static String ACTION_DEFAULT     = "RESTORE_DEFAULT";
  private final static String ACTION_VALIDATE    = "VALIDATE";

  public final static Integer WINDOW_VERSION = Integer.valueOf(2);

  public final List<OptionsPanel> panels = new ArrayList<OptionsPanel>();

  private JButton buttonApply;
  private JButton buttonCancel;
  private JButton buttonDefault;
  private JButton buttonValidate;

  /**
   * Create and display an OptionsWindow.
   */
  public static void createOptionsWindow() {
    createWindow(
        "OptionsWindow",
        null,
        WindowConstants.DISPOSE_ON_CLOSE,
        OptionsWindow.class,
        null);
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._("Options");
  }

  /**
   * @return Window components.
   */
  @Override
  protected Component createComponents() {
    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    panel.add(createOptionsComponents());
    panel.add(createCommandComponents());
    return panel;
  }

  /**
   * @return Login components.
   */
  private Component createOptionsComponents() {
    JPanel panel = new JPanel(new BorderLayout());

    // Create tabbed pane
    JTabbedPane pane = new JTabbedPane();
    GeneralOptionsPanel generalOptions = new GeneralOptionsPanel();
    pane.addTab(GT._("General"), generalOptions);
    panels.add(generalOptions);
    AnalysisOptionsPanel analysisOptions = new AnalysisOptionsPanel();
    pane.addTab(GT._("Full analysis"), analysisOptions);
    panels.add(analysisOptions);
    SortingOptionsPanel sortingOptions = new SortingOptionsPanel();
    pane.addTab(GT._("Sorting"), sortingOptions);
    panels.add(sortingOptions);
    DebugOptionsPanel debugOptions = new DebugOptionsPanel();
    pane.addTab(GT._("Debug"), debugOptions);
    panels.add(debugOptions);
    panel.add(pane);

    return panel;
  }

  /**
   * @return Login components.
   */
  private Component createCommandComponents() {
    JPanel panel = new JPanel(new FlowLayout(FlowLayout.CENTER));

    // Apply button
    buttonApply = Utilities.createJButton(GT._("&Apply"));
    buttonApply.setActionCommand(ACTION_APPLY);
    buttonApply.addActionListener(this);
    panel.add(buttonApply);

    // Validate button
    buttonValidate = Utilities.createJButton(GT._("&Validate"));
    buttonValidate.setActionCommand(ACTION_VALIDATE);
    buttonValidate.addActionListener(this);
    panel.add(buttonValidate);

    // Cancel button
    buttonCancel = Utilities.createJButton(GT._("&Cancel"));
    buttonCancel.setActionCommand(ACTION_CANCEL);
    buttonCancel.addActionListener(this);
    panel.add(buttonCancel);

    // Restore defaults button
    buttonDefault = Utilities.createJButton(GT._("&Restore defaults"));
    buttonDefault.setActionCommand(ACTION_DEFAULT);
    buttonDefault.addActionListener(this);
    panel.add(buttonDefault);

    return panel;
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

    if (ACTION_APPLY.equals(e.getActionCommand())) {
      actionApply();
    } else if (ACTION_VALIDATE.equals(e.getActionCommand())) {
      actionValidate();
    } else if (ACTION_CANCEL.equals(e.getActionCommand())) {
      actionCancel();
    } else if (ACTION_DEFAULT.equals(e.getActionCommand())) {
      actionDefault();
    }
  }

  /**
   * Action called when Apply button is pressed.
   */
  private void actionApply() {
    for (OptionsPanel panel : panels) {
      panel.apply();
    }
    Configuration config = Configuration.getConfiguration();
    config.updateConfiguration();
  }

  /**
   * Action called when Default button is pressed.
   */
  private void actionDefault() {
    for (OptionsPanel panel : panels) {
      panel.defaultValues();
    }
  }

  /**
   * Action called when Validate button is pressed.
   */
  private void actionValidate() {
    actionApply();
    dispose();
  }

  /**
   * Action called when Cancel button is pressed.
   */
  private void actionCancel() {
    dispose();
  }
}
