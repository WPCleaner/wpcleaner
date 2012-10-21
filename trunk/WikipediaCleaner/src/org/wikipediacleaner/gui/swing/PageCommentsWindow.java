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
import java.awt.event.ActionListener;
import java.beans.EventHandler;
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
public class PageCommentsWindow extends BasicWindow {

  Page   page;
  private Integer countMain;
  private Integer countOther;
  private Integer countTemplate;

  private JTextField txtComments;
  private JFormattedTextField txtMaxMain;
  private JFormattedTextField txtMaxOther;
  private JFormattedTextField txtMaxTemplate;

  private JButton buttonCancel;
  private JButton buttonCopyMaxMain;
  private JButton buttonCopyMaxOther;
  private JButton buttonCopyMaxTemplate;
  private JButton buttonOk;
  private JButton buttonRemove;

  public final static Integer WINDOW_VERSION = Integer.valueOf(2);

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
    constraints.gridwidth = 3;
    constraints.gridx++;
    constraints.weightx = 1;
    panel.add(txtComments, constraints);
    constraints.gridwidth = 1;
    constraints.gridy++;

    // Maximum number of backlinks
    txtMaxMain = new JFormattedTextField(NumberFormat.getIntegerInstance());
    txtMaxMain.setFocusLostBehavior(JFormattedTextField.COMMIT);
    txtMaxMain.setColumns(4);
    if ((page != null) &&
        (page.getComment() != null) &&
        (page.getComment().getMaxMainArticles() != null)) {
      txtMaxMain.setValue(page.getComment().getMaxMainArticles());
    }
    JLabel labelMaxMain = Utilities.createJLabel(GT._("Max backlinks in Main :"));
    labelMaxMain.setLabelFor(txtMaxMain);
    labelMaxMain.setHorizontalAlignment(SwingConstants.TRAILING);
    countMain = (page != null) ? page.getBacklinksCountInMainNamespace() : null;
    JLabel labelMain = new JLabel((countMain != null) ? "/ " + countMain.toString() : "");
    labelMain.setHorizontalAlignment(SwingConstants.LEADING);
    buttonCopyMaxMain = Utilities.createJButton("\u21D0");
    buttonCopyMaxMain.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionCopyMain"));
    buttonCopyMaxMain.setEnabled(countMain != null);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(labelMaxMain, constraints);
    constraints.gridx++;
    constraints.weightx = 0;
    panel.add(txtMaxMain, constraints);
    constraints.gridx++;
    constraints.weightx = 0;
    panel.add(buttonCopyMaxMain, constraints);
    constraints.gridx++;
    constraints.weightx = 1;
    panel.add(labelMain, constraints);
    constraints.gridy++;

    txtMaxTemplate = new JFormattedTextField(NumberFormat.getIntegerInstance());
    txtMaxTemplate.setFocusLostBehavior(JFormattedTextField.COMMIT);
    txtMaxTemplate.setColumns(4);
    if ((page != null) &&
        (page.getComment() != null) &&
        (page.getComment().getMaxTemplateArticles() != null)) {
      txtMaxTemplate.setValue(page.getComment().getMaxTemplateArticles());
    }
    JLabel labelMaxTemplate = Utilities.createJLabel(GT._("Max backlinks in Template :"));
    labelMaxTemplate.setLabelFor(txtMaxTemplate);
    labelMaxTemplate.setHorizontalAlignment(SwingConstants.TRAILING);
    countTemplate = (page != null) ? page.getBacklinksCountInTemplateNamespace() : null;
    JLabel labelTemplate = new JLabel((countTemplate != null) ? "/ " + countTemplate.toString() : "");
    labelTemplate.setHorizontalAlignment(SwingConstants.LEADING);
    buttonCopyMaxTemplate = Utilities.createJButton("\u21D0");
    buttonCopyMaxTemplate.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionCopyTemplate"));
    buttonCopyMaxTemplate.setEnabled(countTemplate != null);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(labelMaxTemplate, constraints);
    constraints.gridx++;
    constraints.weightx = 0;
    panel.add(txtMaxTemplate, constraints);
    constraints.gridx++;
    constraints.weightx = 0;
    panel.add(buttonCopyMaxTemplate, constraints);
    constraints.gridx++;
    constraints.weightx = 1;
    panel.add(labelTemplate, constraints);
    constraints.gridy++;

    txtMaxOther = new JFormattedTextField(NumberFormat.getIntegerInstance());
    txtMaxOther.setFocusLostBehavior(JFormattedTextField.COMMIT);
    txtMaxOther.setColumns(4);
    if ((page != null) &&
        (page.getComment() != null) &&
        (page.getComment().getMaxOtherArticles() != null)) {
      txtMaxOther.setValue(page.getComment().getMaxOtherArticles());
    }
    JLabel labelMaxOther = Utilities.createJLabel(GT._("Max backlinks in other namespaces :"));
    labelMaxOther.setLabelFor(txtMaxOther);
    labelMaxOther.setHorizontalAlignment(SwingConstants.TRAILING);
    if (page.getBacklinksCount() != null) {
      int count = page.getBacklinksCount().intValue();
      if (page.getBacklinksCountInMainNamespace() != null) {
        count -= page.getBacklinksCountInMainNamespace().intValue();
      }
      if (page.getBacklinksCountInTemplateNamespace() != null) {
        count -= page.getBacklinksCountInTemplateNamespace().intValue();
      }
      countOther = Integer.valueOf(count);
    } else {
      countOther = null;
    }
    JLabel labelOther = new JLabel((countOther != null) ? "/ " + countOther.toString() : "");
    labelOther.setHorizontalAlignment(SwingConstants.LEADING);
    buttonCopyMaxOther = Utilities.createJButton("\u21D0");
    buttonCopyMaxOther.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionCopyOther"));
    buttonCopyMaxOther.setEnabled(countOther != null);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(labelMaxOther, constraints);
    constraints.gridx++;
    constraints.weightx = 0;
    panel.add(txtMaxOther, constraints);
    constraints.gridx++;
    constraints.weightx = 0;
    panel.add(buttonCopyMaxOther, constraints);
    constraints.gridx++;
    constraints.weightx = 1;
    panel.add(labelOther, constraints);
    constraints.gridy++;

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
    buttonOk.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionOk"));
    panel.add(buttonOk);

    // Validate button
    buttonRemove = Utilities.createJButton(GT._("&Remove page comments"));
    buttonRemove.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionRemove"));
    panel.add(buttonRemove);

    // Cancel button
    buttonCancel = Utilities.createJButton(GT._("&Cancel"));
    buttonCancel.addActionListener(EventHandler.create(
        ActionListener.class, this, "dispose"));
    panel.add(buttonCancel);

    return panel;
  }

  /**
   * Action called when Copy Main button is pressed.
   */
  public void actionCopyMain() {
    if (countMain != null) {
      txtMaxMain.setText(countMain.toString());
    }
  }

  /**
   * Action called when Copy Template button is pressed.
   */
  public void actionCopyTemplate() {
    if (countTemplate != null) {
      txtMaxTemplate.setText(countTemplate.toString());
    }
  }

  /**
   * Action called when Copy Other button is pressed.
   */
  public void actionCopyOther() {
    if (countOther != null) {
      txtMaxOther.setText(countOther.toString());
    }
  }

  /**
   * Action called when OK button is pressed.
   */
  public void actionOk() {
    if (page != null) {
      PageComment comment = page.getComment();
      if (comment == null) {
        comment = new PageComment();
      }
      comment.setComment(txtComments.getText());
      try {
        comment.setMaxMainArticles(Integer.valueOf(txtMaxMain.getText()));
      } catch (NumberFormatException e) {
        comment.setMaxMainArticles(null);
      }
      try {
        comment.setMaxTemplateArticles(Integer.valueOf(txtMaxTemplate.getText()));
      } catch (NumberFormatException e) {
        comment.setMaxTemplateArticles(null);
      }
      try {
        comment.setMaxOtherArticles(Integer.valueOf(txtMaxOther.getText()));
      } catch (NumberFormatException e) {
        comment.setMaxOtherArticles(null);
      }
      page.setComment(comment);
      Configuration config = Configuration.getConfiguration();
      config.addPojo(page.getWikipedia(), Configuration.POJO_PAGE_COMMENTS, comment, page.getTitle());
    }
    dispose();
  }

  /**
   * Action called when Remove button is pressed.
   */
  public void actionRemove() {
    if (page != null) {
      page.setComment(null);
      Configuration config = Configuration.getConfiguration();
      config.removePojo(page.getWikipedia(), Configuration.POJO_PAGE_COMMENTS, page.getTitle());
    }
    dispose();
  }
}
