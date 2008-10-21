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
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.NumberFormat;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageComment;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;


/**
 * Page Comments Window of WikipediaCleaner. 
 */
public class PageCommentsWindow extends BasicWindow implements ActionListener {

  private final static String ACTION_CANCEL   = "CANCEL";
  private final static String ACTION_OK       = "OK";
  private final static String ACTION_REMOVE   = "REMOVE";

  Page   page;

  private JTextField txtComments;
  private JFormattedTextField txtMaxMain;
  private JFormattedTextField txtMax;

  private JButton buttonCancel;
  private JButton buttonOk;
  private JButton buttonRemove;

  /**
   * Create and display a PageCommentsWindow.
   * 
   * @param page Page.
   * @param wikipedia Wikipedia.
   */
  public static void createPageCommentsWindow(
      final Page    page,
      EnumWikipedia wikipedia) {
    createWindow(
        "PageCommentsWindow",
        wikipedia,
        WindowConstants.DISPOSE_ON_CLOSE,
        PageCommentsWindow.class,
        new DefaultBasicWindowListener() {
          @Override
          public void initializeWindow(BasicWindow window) {
            if (window instanceof PageCommentsWindow) {
              PageCommentsWindow pageComments = (PageCommentsWindow) window;
              pageComments.page = page;
            }
          }
        });
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._("Page comments - {0}", page.getTitle());
  }

  /**
   * @return Window components.
   */
  @Override
  protected Component createComponents() {
    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    panel.add(createCommentsComponents());
    panel.add(createCommandComponents());
    return panel;
  }

  /**
   * @return Comments components.
   */
  private Component createCommentsComponents() {
    JPanel panel = new JPanel(new GridBagLayout());
    panel.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("Comments")));

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

    // Comment
    txtComments = new JTextField(20);
    if ((page != null) && (page.getComment() != null)) {
      txtComments.setText(page.getComment().getComment());
    }
    JLabel labelComments = Utilities.createJLabel(GT._("Comments :"));
    labelComments.setLabelFor(txtComments);
    labelComments.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(labelComments, constraints);
    constraints.gridwidth = 5;
    constraints.gridx++;
    constraints.weightx = 1;
    panel.add(txtComments, constraints);
    constraints.gridwidth = 1;
    constraints.gridy++;

    // Maximum number of backlinks
    txtMaxMain = new JFormattedTextField(NumberFormat.getIntegerInstance());
    txtMaxMain.setFocusLostBehavior(JFormattedTextField.COMMIT);
    if ((page != null) &&
        (page.getComment() != null) &&
        (page.getComment().getMaxMainArticles() != null)) {
      txtMaxMain.setValue(page.getComment().getMaxMainArticles());
    }
    JLabel labelMaxMain = Utilities.createJLabel(GT._("Max backlinks in Main :"));
    labelMaxMain.setLabelFor(txtMaxMain);
    labelMaxMain.setHorizontalAlignment(SwingConstants.TRAILING);
    Integer main = (page != null) ? page.getBacklinksCountInMainNamespace() : null;
    JLabel labelMain = new JLabel((main != null) ? "/ " + main.toString() : "");
    labelMain.setHorizontalAlignment(SwingConstants.LEADING);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(labelMaxMain, constraints);
    constraints.gridx++;
    constraints.weightx = 0.5;
    panel.add(txtMaxMain, constraints);
    constraints.gridx++;
    constraints.weightx = 0;
    panel.add(labelMain, constraints);
    txtMax = new JFormattedTextField(NumberFormat.getIntegerInstance());
    txtMax.setFocusLostBehavior(JFormattedTextField.COMMIT);
    if ((page != null) &&
        (page.getComment() != null) &&
        (page.getComment().getMaxArticles() != null)) {
      txtMax.setValue(page.getComment().getMaxArticles());
    }
    JLabel labelMax = Utilities.createJLabel(GT._("Max backlinks :"));
    labelMax.setLabelFor(txtMax);
    labelMax.setHorizontalAlignment(SwingConstants.TRAILING);
    Integer all = (page != null) ? page.getBacklinksCount() : null;
    JLabel labelAll = new JLabel((all != null) ? "/ " + all.toString() : "");
    labelAll.setHorizontalAlignment(SwingConstants.LEADING);
    constraints.gridx++;
    constraints.weightx = 0;
    panel.add(labelMax, constraints);
    constraints.gridx++;
    constraints.weightx = 0.5;
    panel.add(txtMax, constraints);
    constraints.gridx++;
    constraints.weightx = 0;
    panel.add(labelAll, constraints);

    // Empty panel
    JPanel emptyPanel = new JPanel();
    emptyPanel.setMinimumSize(new Dimension(0, 0));
    emptyPanel.setPreferredSize(new Dimension(0, 0));
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.weightx = 1;
    constraints.weighty = 1;
    panel.add(emptyPanel, constraints);

    return panel;
  }

  /**
   * @return Login components.
   */
  private Component createCommandComponents() {
    JPanel panel = new JPanel(new FlowLayout(FlowLayout.CENTER));
    panel.setBorder(BorderFactory.createEtchedBorder());

    // Ok button
    buttonOk = Utilities.createJButton(GT._("&OK"));
    buttonOk.setActionCommand(ACTION_OK);
    buttonOk.addActionListener(this);
    panel.add(buttonOk);

    // Validate button
    buttonRemove = Utilities.createJButton(GT._("&Remove page comments"));
    buttonRemove.setActionCommand(ACTION_REMOVE);
    buttonRemove.addActionListener(this);
    panel.add(buttonRemove);

    // Cancel button
    buttonCancel = Utilities.createJButton(GT._("&Cancel"));
    buttonCancel.setActionCommand(ACTION_CANCEL);
    buttonCancel.addActionListener(this);
    panel.add(buttonCancel);

    return panel;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(ActionEvent e) {
    if (e == null) {
      return;
    }

    if (ACTION_OK.equals(e.getActionCommand())) {
      actionOk();
    } else if (ACTION_REMOVE.equals(e.getActionCommand())) {
      actionRemove();
    } else if (ACTION_CANCEL.equals(e.getActionCommand())) {
      actionCancel();
    }
  }

  /**
   * Action called when OK button is pressed.
   */
  private void actionOk() {
    if (page != null) {
      PageComment comment = page.getComment();
      if (comment == null) {
        comment = new PageComment();
      }
      comment.setComment(txtComments.getText());
      try {
        comment.setMaxArticles(Integer.valueOf(txtMax.getText()));
      } catch (NumberFormatException e) {
        comment.setMaxArticles(null);
      }
      try {
        comment.setMaxMainArticles(Integer.valueOf(txtMaxMain.getText()));
      } catch (NumberFormatException e) {
        comment.setMaxMainArticles(null);
      }
      page.setComment(comment);
      Configuration config = Configuration.getConfiguration();
      config.addPojo(Configuration.POJO_PAGE_COMMENTS, comment, page.getTitle());
    }
    dispose();
  }

  /**
   * Action called when Cancel button is pressed.
   */
  private void actionCancel() {
    dispose();
  }

  /**
   * Action called when Remove button is pressed.
   */
  private void actionRemove() {
    if (page != null) {
      page.setComment(null);
      Configuration config = Configuration.getConfiguration();
      config.removePojo(Configuration.POJO_PAGE_COMMENTS, page.getTitle());
    }
    dispose();
  }
}
