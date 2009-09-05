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
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenuBar;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.text.StyledDocument;

import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWorkerListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.MediaWikiConstants;
import org.wikipediacleaner.gui.swing.component.MediaWikiPane;
import org.wikipediacleaner.gui.swing.worker.CheckWikiProjectWorker;
import org.wikipediacleaner.gui.swing.worker.RetrieveContentWorker;
import org.wikipediacleaner.i18n.GT;


/**
 * Check Wiki Project window.
 */
public class CheckWikiProjectWindow extends PageWindow {

  private Page projectPage;

  private ArrayList<CheckError> errors;
  private JComboBox listAllErrors;
  private DefaultComboBoxModel modelAllErrors;

  private JList listPages;
  private DefaultListModel modelPages;
  private JCheckBox chkShowFullList;

  private JList listErrors;
  private DefaultListModel modelErrors;

  /**
   * Create and display a CheckWikiProjectWindow.
   * 
   * @param wikipedia Wikipedia.
   */
  public static void createCheckWikiProjectWindow(
      final EnumWikipedia wikipedia) {
    createWindow(
        "CheckWikiWindow",
        wikipedia,
        WindowConstants.DISPOSE_ON_CLOSE,
        CheckWikiProjectWindow.class,
        new DefaultBasicWindowListener() {
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
    return GT._("Check Wikipedia");
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
    projectPage = DataManager.getPage(
        getWikipedia(),
        getWikipedia().getCheckWikiProject(), null);

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

    // TODO
    JLabel labelNotFinished = Utilities.createJLabel(GT._(
        "This screen is not functional yet !"));
    labelNotFinished.setHorizontalAlignment(SwingConstants.CENTER);
    labelNotFinished.setForeground(Color.RED);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 0;
    constraints.weighty = 0;
    panel.add(labelNotFinished, constraints);
    constraints.gridy++;

    // Check Wikipedia Project
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(createProjectComponents(), constraints);
    constraints.gridy++;

    // Page list
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.weightx = 0;
    constraints.weighty = 1;
    panel.add(createPageListComponents(), constraints);

    // Contents
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 1;
    constraints.gridx++;
    constraints.weightx = 1;
    constraints.weighty = 1;
    panel.add(createContentsComponents(), constraints);
    constraints.gridy++;

    updateComponentState();
    return panel;
  }

  /**
   * @return Project components
   */
  private Component createProjectComponents() {
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

    // Comments
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.weightx = 0;
    constraints.weighty = 0;
    addChkAutomaticComment(panel, constraints);
    constraints.gridy++;

    // List of errors managed by the project
    JLabel labelErrors = Utilities.createJLabel(GT._("List of errors detected :"));
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.weightx = 0;
    constraints.weighty = 0;
    panel.add(labelErrors, constraints);
    modelAllErrors = new DefaultComboBoxModel();
    listAllErrors = new JComboBox(modelAllErrors);
    listAllErrors.addActionListener(new ActionListener() {
      public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
        actionSelectErrorType();
      }
    });
    constraints.gridx++;
    constraints.weightx = 1;
    panel.add(listAllErrors, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * @return Page liste components
   */
  private Component createPageListComponents() {
    JPanel panel = new JPanel(new GridBagLayout());
    panel.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("Pages")));

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

    // Load full list
    JButton buttonLoadFullList = Utilities.createJButton(GT._("Load full list"));
    buttonLoadFullList.setEnabled(false); // TODO: Manage action on this button
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridheight = 1;
    constraints.gridx = 0;
    constraints.weightx = 0;
    constraints.weighty = 0;
    panel.add(buttonLoadFullList, constraints);
    constraints.gridy++;

    // Show full list
    chkShowFullList = Utilities.createJCheckBox(GT._("Show full list"), false);
    chkShowFullList.setEnabled(false);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridheight = 1;
    constraints.gridx = 0;
    constraints.weightx = 0;
    constraints.weighty = 0;
    panel.add(chkShowFullList, constraints);
    constraints.gridy++;

    // Page List
    modelPages = new DefaultListModel();
    listPages = new JList(modelPages);
    listPages.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    listPages.addMouseListener(new MouseAdapter() {

      /* (non-Javadoc)
       * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
       */
      @Override
      public void mouseClicked(MouseEvent e) {
        if (e.getButton() != MouseEvent.BUTTON1) {
          return;
        }
        if (e.getClickCount() != 2) {
          return;
        }
        actionSelectPage();
      }
    });
    JScrollPane scrollPages = new JScrollPane(listPages);
    scrollPages.setMinimumSize(new Dimension(200, 200));
    scrollPages.setPreferredSize(new Dimension(200, 300));
    scrollPages.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridx = 0;
    constraints.weightx = 0;
    constraints.weighty = 1;
    panel.add(scrollPages, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * @return Contents components.
   */
  private Component createContentsComponents() {
    JPanel panel = new JPanel(new GridBagLayout());
    panel.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("Selected page")));

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

    // Buttons
    JPanel panelButtons = new JPanel(new FlowLayout(FlowLayout.CENTER));
    addButtonSend(panelButtons);
    addButtonView(panelButtons);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridwidth = 2;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(panelButtons, constraints);
    constraints.gridy++;

    // Errors list
    modelErrors = new DefaultListModel();
    listErrors = new JList(modelErrors);
    listErrors.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    listErrors.addListSelectionListener(new ListSelectionListener() {

      public void valueChanged(ListSelectionEvent e) {
        if (e.getValueIsAdjusting()) {
          return;
        }
        actionSelectError();
      }
      
    });
    JScrollPane scrollErrors = new JScrollPane(listErrors);
    scrollErrors.setMinimumSize(new Dimension(200, 200));
    scrollErrors.setPreferredSize(new Dimension(200, 300));
    scrollErrors.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.weightx = 0;
    constraints.weighty = 1;
    panel.add(scrollErrors, constraints);
    constraints.gridx++;

    // Page contents
    createTextContents(this);
    /*JScrollPane scrollPage = new JScrollPane(getTextContents());
    scrollPage.setMinimumSize(new Dimension(200, 200));
    scrollPage.setPreferredSize(new Dimension(1000, 500));
    scrollPage.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);*/
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weightx = 1;
    constraints.weighty = 1;
    //panel.add(scrollPage, constraints);
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

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.PageWindow#afterFinishedReloadWorker()
   */
  @Override
  protected void afterFinishedReloadWorker() {
    super.afterFinishedReloadWorker();
    analyzeCheckWiki();
  }

  /**
   * Analyze the Check Wiki page contents.
   */
  private void analyzeCheckWiki() {
    String contents = projectPage.getContents();
    errors = CheckError.initCheckErrors(getWikipedia(), contents);
    if (modelAllErrors != null) {
      modelAllErrors.removeAllElements();
      if (errors != null) {
        for (CheckError error : errors) {
          modelAllErrors.addElement(error);
        }
      }
    }
  }

  /**
   * Action called when an error type is selected.
   */
  void actionSelectErrorType() {
    Object selection = listAllErrors.getSelectedItem();
    modelPages.clear();
    if (selection instanceof CheckError) {
      CheckError error = (CheckError) selection;
      chkShowFullList.setEnabled(error.isFullListInitialized());
      if (!error.isFullListInitialized()) {
        chkShowFullList.setSelected(false);
      }
      int nbPages = error.getPageCount(chkShowFullList.isSelected());
      for (int numPage = 0; numPage < nbPages; numPage++) {
        Page page = error.getPage(numPage, chkShowFullList.isSelected());
        modelPages.addElement(page);
      }
      setPageLoaded(false);
      actionSelectPage();
    }
  }

  /**
   * Action called when a page is selected.
   */
  void actionSelectPage() {
    Object selection = listPages.getSelectedValue();
    if (selection instanceof Page) {
      setPage((Page) selection);
      RetrieveContentWorker contentWorker = new RetrieveContentWorker(this, getPage());
      contentWorker.setListener(new DefaultBasicWorkerListener() {

        /* (non-Javadoc)
         * @see org.wikipediacleaner.gui.swing.basic.DefaultBasicWorkerListener#beforeFinished(org.wikipediacleaner.gui.swing.basic.BasicWorker)
         */
        @Override
        public void beforeFinished(BasicWorker worker) {
          setPageLoaded(true);
          super.beforeFinished(worker);
          actionPageSelected();
        }
        //
      });
      contentWorker.start();
    } else {
      setPage(null);
      setPageLoaded(false);
      actionPageSelected();
      updateComponentState();
    }
  }

  /**
   * Action called when a page is selected (after page is loaded).
   */
  void actionPageSelected() {
    MediaWikiPane textPage = getTextContents();
    Page page = getPage();
    if (page != null) {
      textPage.setPage(page);
      textPage.setText(page.getContents());
      textPage.setModified(false);
      ArrayList<CheckErrorAlgorithm> errorsFound = CheckError.analyzeErrors(
          errors, page, page.getContents());
      modelErrors.clear();
      if (errorsFound != null) {
        for (CheckErrorAlgorithm algorithm : errorsFound) {
          modelErrors.addElement(algorithm);
        }
      }
      int index = modelErrors.indexOf(listAllErrors.getSelectedItem());
      if (index >= 0) {
        listErrors.setSelectedIndex(index);
      } else if (modelErrors.getSize() > 0) {
        listErrors.setSelectedIndex(0);
      }
    } else {
      textPage.setPage((Page) null);
      textPage.setText("");
      textPage.setModified(false);
      modelErrors.clear();
    }
  }

  /**
   * Action called when an error is selected. 
   */
  void actionSelectError() {
    Object selection = listErrors.getSelectedValue();
    if (selection instanceof CheckErrorAlgorithm) {
      CheckErrorAlgorithm algorithm = (CheckErrorAlgorithm) selection;
      MediaWikiPane textPage = getTextContents();
      boolean modified = textPage.isModified();
      String contents = textPage.getText();
      ArrayList<CheckErrorResult> errorsFound = CheckError.analyzeError(
          algorithm, getPage(), contents);
      boolean visible = false;
      textPage.resetAttributes();
      StyledDocument document = textPage.getStyledDocument();
      if (document != null) {
        if (errorsFound != null) {
          for (CheckErrorResult error : errorsFound) {
            document.setCharacterAttributes(
                error.getStartPosition(),
                error.getLength(),
                textPage.getStyle(MediaWikiConstants.STYLE_DISAMBIGUATION_LINK),
                true);
            if (!visible) {
              textPage.setCaretPosition(error.getStartPosition());
              textPage.moveCaretPosition(error.getEndPosition());
              visible = true;
            }
          }
        }
      }
      textPage.setModified(modified);
      updateComponentState();
    }
  }

  /**
   * Action called when Reload button is pressed. 
   */
  @Override
  protected void actionReload() {
    clean();
    CheckWikiProjectWorker reloadWorker = new CheckWikiProjectWorker(this, projectPage);
    setupReloadWorker(reloadWorker);
    reloadWorker.start();
  }
}
