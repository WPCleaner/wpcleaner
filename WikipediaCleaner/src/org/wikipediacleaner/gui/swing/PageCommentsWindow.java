/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
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
import java.util.Optional;

import javax.annotation.Nonnull;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.page.PageComment;
import org.wikipediacleaner.gui.swing.action.ActionDispose;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;


/**
 * Page Comments Window of WikipediaCleaner. 
 */
public class PageCommentsWindow extends BasicWindow {

  @Nonnull Page   page;
  @Nonnull PageComment comment;
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
      final Page page,
      final PageComment comment,
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
              pageComments.comment = (comment == null) ? PageComment.get(wikipedia, page.getTitle()).orElse(null) : comment;
            }
          }
        });
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._T("Page comments - {0}", page.getTitle());
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
        BorderFactory.createEtchedBorder(), GT._T("Comments")));

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
    txtComments.setText(Optional.ofNullable(comment).map(PageComment::getComment).orElse(StringUtils.EMPTY));
    JLabel labelComments = Utilities.createJLabel(GT._T("Comments :"));
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
    txtMaxMain.setValue(Optional.ofNullable(comment).map(PageComment::getMaxMainArticles).orElse(null));
    JLabel labelMaxMain = Utilities.createJLabel(GT._T("Max backlinks in Main :"));
    labelMaxMain.setLabelFor(txtMaxMain);
    labelMaxMain.setHorizontalAlignment(SwingConstants.TRAILING);
    countMain = (page != null) ? page.getBacklinksCountInMainNamespace() : null;
    JLabel labelMain = new JLabel((countMain != null) ? "/ " + countMain.toString() : "");
    labelMain.setHorizontalAlignment(SwingConstants.LEADING);
    buttonCopyMaxMain = Utilities.createJButton("\u21D0", null);
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
    txtMaxTemplate.setValue(Optional.ofNullable(comment).map(PageComment::getMaxTemplateArticles).orElse(null));
    JLabel labelMaxTemplate = Utilities.createJLabel(GT._T("Max backlinks in Template :"));
    labelMaxTemplate.setLabelFor(txtMaxTemplate);
    labelMaxTemplate.setHorizontalAlignment(SwingConstants.TRAILING);
    countTemplate = (page != null) ? page.getBacklinksCountInTemplateNamespace() : null;
    JLabel labelTemplate = new JLabel((countTemplate != null) ? "/ " + countTemplate.toString() : "");
    labelTemplate.setHorizontalAlignment(SwingConstants.LEADING);
    buttonCopyMaxTemplate = Utilities.createJButton("\u21D0", null);
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
    txtMaxOther.setValue(Optional.ofNullable(comment).map(PageComment::getMaxOtherArticles).orElse(null));
    JLabel labelMaxOther = Utilities.createJLabel(GT._T("Max backlinks in other namespaces :"));
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
    buttonCopyMaxOther = Utilities.createJButton("\u21D0", null);
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
    buttonOk = Utilities.createJButton(GT._T("&OK"), null);
    buttonOk.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionOk"));
    panel.add(buttonOk);

    // Validate button
    buttonRemove = Utilities.createJButton(GT._T("&Remove page comments"), null);
    buttonRemove.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionRemove"));
    panel.add(buttonRemove);

    // Cancel button
    buttonCancel = ActionDispose.createButton(getParentComponent(), true, true);
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
      if (comment == null) {
        comment = PageComment.getOrCreate(getWiki(), page.getTitle());
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
      comment.save();
    }
    dispose();
  }

  /**
   * Action called when Remove button is pressed.
   */
  public void actionRemove() {
    if (page != null) {
      PageComment.delete(page.getWikipedia(), page.getTitle());
    }
    if (comment != null) {
      comment.setComment(null);
      comment.setMaxMainArticles(null);
      comment.setMaxTemplateArticles(null);
      comment.setMaxOtherArticles(null);
    }
    dispose();
  }
}
