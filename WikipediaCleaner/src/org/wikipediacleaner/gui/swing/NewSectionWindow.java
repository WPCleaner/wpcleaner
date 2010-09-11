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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

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
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.worker.NewSectionWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;


/**
 * A window to create a new section.
 */
public class NewSectionWindow extends BasicWindow implements ActionListener {

  private final static String ACTION_CANCEL    = "CANCEL";
  private final static String ACTION_SIGNATURE = "SIGNATURE";
  private final static String ACTION_VALIDATE  = "VALIDATE";

  public final static Integer WINDOW_VERSION = Integer.valueOf(2);

  Page page;
  String articleText;

  private JTextField textTitle;
  private JCheckBox chkForceWatch;
  private JButton   buttonSignature;
  private JTextPane textNewSection;
  private JTextPane textArticle;

  private JButton buttonValidate;
  private JButton buttonCancel;

  /**
   * Create and display a NewSectionWindow.
   * 
   * @param page Page name.
   * @param articleText Text of the article.
   * @param wikipedia Wikipedia.
   */
  public static void createNewSectionWindow(
      final Page page,
      final String articleText,
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
            }
          }
        });
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._("New section in {0}", page.getTitle());
  }

  /**
   * @return Window components.
   */
  @Override
  protected Component createComponents() {
    JPanel panel = new JPanel(new GridBagLayout());
    JPanel panelComment = new JPanel(new GridBagLayout());
    panelComment.setBorder(new TitledBorder(GT._("Comment")));

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
    JLabel labelTitle = Utilities.createJLabel(GT._("&Title"));
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
    buttonSignature = Utilities.createJButton(GT._("&Signature"));
    buttonSignature.setActionCommand(ACTION_SIGNATURE);
    buttonSignature.addActionListener(this);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panelComment.add(buttonSignature, constraints);
    chkForceWatch = Utilities.createJCheckBox(
        GT._("&Force watching page"), false);
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
      textArticle = new JTextPane();
      textArticle.setEditable(false);
      textArticle.setText(articleText);
      textArticle.setCaretPosition(0);
      JScrollPane scrollText = new JScrollPane(textArticle);
      scrollText.setMinimumSize(new Dimension(100, 100));
      scrollText.setPreferredSize(new Dimension(1000, 300));
      scrollText.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
      JPanel panelText = new JPanel(new BorderLayout());
      panelText.setBorder(new TitledBorder(GT._("Article contents")));
      panelText.add(scrollText);
      panel.add(panelText, constraints);
      constraints.gridy++;
    }

    // Buttons
    JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    buttonValidate = Utilities.createJButton(GT._("&Validate"));
    buttonValidate.setActionCommand(ACTION_VALIDATE);
    buttonValidate.addActionListener(this);
    buttonPanel.add(buttonValidate);
    buttonCancel = Utilities.createJButton(GT._("&Cancel"));
    buttonCancel.setActionCommand(ACTION_CANCEL);
    buttonCancel.addActionListener(this);
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
   * Invoked when an action occurs.
   * 
   * @param e Event.
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if (e == null) {
      return;
    }

    if (ACTION_VALIDATE.equals(e.getActionCommand())) {
      actionValidate();
    } else if (ACTION_CANCEL.equals(e.getActionCommand())) {
      actionCancel();
    } else if (ACTION_SIGNATURE.equals(e.getActionCommand())) {
      actionSignature();
    }
  }

  /**
   * Action called when Validate button is pressed.
   */
  private void actionValidate() {
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
   * Action called when Cancel button is pressed.
   */
  private void actionCancel() {
    dispose();
  }

  /**
   * Action called when Signature button is pressed.
   */
  private void actionSignature() {
    Configuration config = Configuration.getConfiguration();
    try {
      textNewSection.getDocument().insertString(
          textNewSection.getCaretPosition(),
          config.getString(
              Configuration.STRING_SIGNATURE,
              Configuration.DEFAULT_SIGNATURE),
          null);
    } catch (BadLocationException e) {
      //
    }
  }
}
