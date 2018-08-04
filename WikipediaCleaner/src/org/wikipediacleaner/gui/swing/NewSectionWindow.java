/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.beans.EventHandler;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;
import javax.swing.border.TitledBorder;
import javax.swing.text.BadLocationException;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.gui.swing.component.MWPaneBasicFormatter;
import org.wikipediacleaner.gui.swing.worker.NewSectionWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * A window to create a new section.
 */
public class NewSectionWindow extends BasicWindow {

  public final static Integer WINDOW_VERSION = Integer.valueOf(2);

  Page page;
  Page articlePage;
  String articleText;

  private JTextField textTitle;
  private JCheckBox chkForceWatch;
  private JButton   buttonSignature;
  private JTextPane textNewSection;
  private MWPane textArticle;

  private JButton buttonValidate;
  private JButton buttonCancel;

  /**
   * Create and display a NewSectionWindow.
   * 
   * @param page Page name.
   * @param articleText Text of the article.
   * @param articleTitle Title of the article.
   * @param wikipedia Wikipedia.
   */
  public static void createNewSectionWindow(
      final Page page,
      final String articleText,
      final String articleTitle,
      final EnumWikipedia wikipedia) {
    createWindow(
        "NewSectionWindow",
        wikipedia,
        WindowConstants.DISPOSE_ON_CLOSE,
        NewSectionWindow.class,
        new DefaultBasicWindowListener() {
          @Override
          public void initializeWindow(BasicWindow window) {
            if (window instanceof NewSectionWindow) {
              NewSectionWindow newSection = (NewSectionWindow) window;
              newSection.page = page;
              newSection.articleText = articleText;
              newSection.articlePage = DataManager.getPage(wikipedia, articleTitle, null, null, null);
              newSection.articlePage.setContents(articleText);
            }
          }
        });
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._T("New section in {0}", page.getTitle());
  }

  /**
   * @return Window components.
   */
  @Override
  protected Component createComponents() {
    JPanel panel = new JPanel(new GridBagLayout());
    JPanel panelComment = new JPanel(new GridBagLayout());
    panelComment.setBorder(new TitledBorder(GT._T("Comment")));

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

    // Discussion title
    textTitle = new JTextField(60);
    JLabel labelTitle = Utilities.createJLabel(GT._T("&Title"));
    labelTitle.setLabelFor(textTitle);
    labelTitle.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panelComment.add(labelTitle, constraints);
    constraints.gridx++;
    constraints.weightx = 1;
    panelComment.add(textTitle, constraints);
    constraints.gridy++;

    // Force watching and signature
    buttonSignature = Utilities.createJButton(GT._T("&Signature"), null);
    buttonSignature.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionSignature"));
    constraints.gridx = 0;
    constraints.weightx = 0;
    panelComment.add(buttonSignature, constraints);
    chkForceWatch = Utilities.createJCheckBox(
        GT._T("&Force watching page"), false);
    constraints.gridx++;
    constraints.weightx = 1;
    panelComment.add(chkForceWatch, constraints);
    constraints.gridy++;

    // Contents
    textNewSection = new JTextPane();
    textNewSection.setBackground(Color.WHITE);
    textNewSection.setEditable(true);
    JScrollPane scrollContents = new JScrollPane(textNewSection);
    scrollContents.setMinimumSize(new Dimension(100, 100));
    scrollContents.setPreferredSize(new Dimension(1000, 500));
    scrollContents.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weighty = 1;
    constraints.weightx = 1;
    panelComment.add(scrollContents, constraints);
    constraints.gridy++;

    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.weightx = 1;
    constraints.weighty = 1;
    panel.add(panelComment, constraints);
    constraints.gridy++;

    // Article contents
    if ((articleText != null) && (articleText.length() > 0)) {
      textArticle = new MWPane(getWikipedia(), articlePage, this);
      textArticle.setText(articleText);
      textArticle.setFormatter(new MWPaneBasicFormatter());
      textArticle.setEditable(false);
      textArticle.resetAttributes();
      JScrollPane scrollText = new JScrollPane(textArticle);
      scrollText.setMinimumSize(new Dimension(100, 100));
      scrollText.setPreferredSize(new Dimension(1000, 300));
      scrollText.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
      JPanel panelText = new JPanel(new BorderLayout());
      panelText.setBorder(new TitledBorder(GT._T("Article contents")));
      panelText.add(scrollText);
      panel.add(panelText, constraints);
      constraints.gridy++;
    }

    // Buttons
    JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    buttonValidate = Utilities.createJButton(GT._T("&Validate"), null);
    buttonValidate.addActionListener(
        EventHandler.create(ActionListener.class, this, "actionValidate"));
    buttonPanel.add(buttonValidate);
    buttonCancel = Utilities.createJButton(GT._T("&Cancel"), null);
    buttonCancel.addActionListener(EventHandler.create(
        ActionListener.class, this, "dispose"));
    buttonPanel.add(buttonCancel);
    constraints.fill = GridBagConstraints.NONE;
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(buttonPanel, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * Action called when Validate button is pressed.
   */
  public void actionValidate() {
    String section = textTitle.getText().trim();
    String text = textNewSection.getText().trim();
    if (("".equals(section)) || ("".equals(text))) {
      return;
    }
    new NewSectionWorker(
        getWikipedia(), this,
        page, section, text,
        chkForceWatch.isSelected()).start();
  }

  /**
   * Action called when Signature button is pressed.
   */
  public void actionSignature() {
    Configuration config = Configuration.getConfiguration();
    try {
      textNewSection.getDocument().insertString(
          textNewSection.getCaretPosition(),
          config.getString(
              null,
              ConfigurationValueString.SIGNATURE),
          null);
    } catch (BadLocationException e) {
      //
    }
  }
}
