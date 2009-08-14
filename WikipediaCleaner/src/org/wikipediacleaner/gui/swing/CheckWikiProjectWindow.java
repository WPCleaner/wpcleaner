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

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JMenuBar;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.worker.CheckWikiProjectWorker;
import org.wikipediacleaner.i18n.GT;


/**
 * Check Wiki Project window.
 */
public class CheckWikiProjectWindow extends PageWindow {

  /**
   * Create and display a CheckWikiProjectWindow.
   * 
   * @param wikipedia Wikipedia.
   */
  public static void createCheckWikiProjectWindow(
      final EnumWikipedia wikipedia) {
    createWindow(
        "AnalysisWindow",
        wikipedia,
        WindowConstants.DISPOSE_ON_CLOSE,
        CheckWikiProjectWindow.class,
        new DefaultBasicWindowListener() {
          @Override
          public void initializeWindow(BasicWindow window) {
            if (window instanceof CheckWikiProjectWindow) {
              CheckWikiProjectWindow analysis = (CheckWikiProjectWindow) window;
              analysis.setPageName(wikipedia.getCheckWikiProject());
              analysis.createTextContents(window);
            }
          }
          @Override
          public void displayWindow(BasicWindow window) {
            if (window instanceof CheckWikiProjectWindow) {
              CheckWikiProjectWindow analysis = (CheckWikiProjectWindow) window;
              analysis.actionReload();
            }
          }
        });
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._("Project - {0}", getPageName());
  }

  /**
   * @return Menu bar.
   */
  @Override
  protected JMenuBar createMenuBar() {
    JMenuBar menuBar = new JMenuBar();
    menuBar.add(createToolsMenu());
    menuBar.add(Box.createHorizontalGlue());
    return menuBar;
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

    // Page name
    //constraints.gridwidth = 2;
    panel.add(createPageComponents(), constraints);
    constraints.gridy++;

    // Contents
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 1;
    panel.add(createContentsComponents(), constraints);
    constraints.gridy++;

    updateComponentState();
    return panel;
  }

  /**
   * @return Page components.
   */
  private Component createPageComponents() {
    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    JPanel panelInformation = new JPanel(new FlowLayout(FlowLayout.CENTER, 5, 0));
    JPanel panelComment = new JPanel(new GridBagLayout());

    // Check box for closing after sending
    addChkCloseAfterSend(panelInformation);

    // Check box for adding a note on the talk page
    addChkEditTalkPage(panelInformation);

    // Comment
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
    addChkAutomaticComment(panelComment, constraints);

    panel.add(panelInformation);
    panel.add(panelComment);

    return panel;
  }

  /**
   * @return Contents components.
   */
  private Component createContentsComponents() {
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
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Command buttons
    JPanel panelCommand = new JPanel(new FlowLayout(FlowLayout.LEFT, 1, 0));
    addButtonReload(panelCommand);
    addButtonView(panelCommand);
    addButtonSend(panelCommand);
    addButtonWatch(panelCommand);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(panelCommand);
    constraints.gridy++;

    // Contents
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 1;
    addTextContents(panel, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * @return Default comment.
   */
  @Override
  protected String getDefaultComment() {
    return "[[" + getWikipedia().getCheckWikiProject() + "]]";
  }

  /**
   * Action called when Reload button is pressed. 
   */
  @Override
  protected void actionReload() {
    clean();
    CheckWikiProjectWorker reloadWorker = new CheckWikiProjectWorker(this, getPage());
    setupReloadWorker(reloadWorker);
    reloadWorker.start();
  }
}
