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
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.concurrent.Callable;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenuBar;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyledDocument;

import org.wikipediacleaner.api.MediaWikiController;
import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
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
import org.wikipediacleaner.gui.swing.worker.SendWorker;
import org.wikipediacleaner.i18n.GT;


/**
 * Check Wiki Project window.
 */
public class CheckWikiProjectWindow extends PageWindow {

  ArrayList<CheckError> errors;
  JComboBox listAllErrors;
  private DefaultComboBoxModel modelAllErrors;

  private JList listPages;
  private DefaultListModel modelPages;

  private JTabbedPane contentPane;

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
    contentPane = new JTabbedPane();
    contentPane.setPreferredSize(new Dimension(900, 600));
    panel.add(contentPane, constraints);
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
   * @param page Page.
   * @return Contents components.
   */
  public CheckWikiContentPanel createContentsComponents(JTabbedPane pane, Page page, CheckError error) {
    CheckWikiContentPanel panel = new CheckWikiContentPanel(pane, page, error);
    panel.initialize();
    return panel;
  }
  
  /**
   * Component for working on a page in the CheckWiki project.
   */
  private class CheckWikiContentPanel
    extends JPanel
    implements ActionListener, ItemListener {

    private static final long serialVersionUID = 1L;

    public final static String ACTION_MARK_AS_FIXED = "MARK_AS_FIXED";

    JTabbedPane pane;
    final Page page;
    private final CheckError error;

    private JList listErrors;
    private DefaultListModel modelErrors;
    private JTextField textComment;
    private JCheckBox chkAutomaticComment;
    private JButton buttonSend;
    private MediaWikiPane textPage;

    /**
     * @param page Page.
     */
    CheckWikiContentPanel(JTabbedPane pane, Page page, CheckError error) {
      super(new GridBagLayout());
      this.pane = pane;
      this.page = page;
      this.error = error;
      setName(page.getTitle());
    }

    /**
     * Initialize window.
     */
    void initialize() {

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

      // Comment
      JPanel panelComment = new JPanel(new GridBagLayout());
      GridBagConstraints constraints2 = new GridBagConstraints();
      constraints2.fill = GridBagConstraints.HORIZONTAL;
      constraints2.gridheight = 1;
      constraints2.gridwidth = 1;
      constraints2.gridx = 0;
      constraints2.gridy = 0;
      constraints2.insets = new Insets(0, 0, 0, 0);
      constraints2.ipadx = 0;
      constraints2.ipady = 0;
      constraints2.weightx = 0;
      constraints2.weighty = 0;
      chkAutomaticComment = createChkAutomaticComment(true, this);
      panelComment.add(chkAutomaticComment, constraints2);
      constraints2.gridx++;
      textComment = new JTextField(getComment(null));
      textComment.setEditable(false);
      constraints2.weightx = 1;
      panelComment.add(textComment, constraints2);
      constraints2.gridx++;
      constraints.fill = GridBagConstraints.HORIZONTAL;
      constraints.gridwidth = 2;
      constraints.weightx = 1;
      constraints.weighty = 0;
      add(panelComment, constraints);
      constraints.gridy++;

      // Buttons
      JPanel panelButtons = new JPanel(new FlowLayout(FlowLayout.CENTER));
      JButton buttonValidate = createButtonValidate(this);
      panelButtons.add(buttonValidate);
      buttonSend = createButtonSend(this);
      buttonSend.setEnabled(false);
      panelButtons.add(buttonSend);
      if (Utilities.isDesktopSupported()) { // External Viewer
        JButton buttonView = createButtonView(this);
        panelButtons.add(buttonView);
      }
      JButton buttonMarkAsFixed = Utilities.createJButton(GT._("Mark as Fixed")); // Mark as fixed
      buttonMarkAsFixed.setEnabled(true);
      buttonMarkAsFixed.setActionCommand(ACTION_MARK_AS_FIXED);
      buttonMarkAsFixed.addActionListener(this);
      panelButtons.add(buttonMarkAsFixed);
      JButton buttonFullAnalysis = createButtonFullAnalysis(this);
      panelButtons.add(buttonFullAnalysis);
      constraints.fill = GridBagConstraints.HORIZONTAL;
      constraints.gridwidth = 2;
      constraints.weightx = 1;
      constraints.weighty = 0;
      add(panelButtons, constraints);
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
      add(scrollErrors, constraints);
      constraints.gridx++;

      // Page contents
      textPage = new MediaWikiPane(getWikipedia(), page, CheckWikiProjectWindow.this);
      textPage.setEditable(true);
      textPage.addPropertyChangeListener(
          MediaWikiPane.PROPERTY_MODIFIED,
          new PropertyChangeListener() {
  
            /* (non-Javadoc)
             * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
             */
            public void propertyChange(@SuppressWarnings("unused") PropertyChangeEvent evt) {
              updateComponentState();
            }
            
          });
      JScrollPane scrollContents = new JScrollPane(textPage);
      scrollContents.setMinimumSize(new Dimension(100, 100));
      scrollContents.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
      constraints.fill = GridBagConstraints.BOTH;
      constraints.weightx = 1;
      constraints.weighty = 1;
      //panel.add(scrollPage, constraints);
      add(scrollContents, constraints);
      constraints.gridy++;
    }

    /**
     * Update component state.
     */
    public void updateComponentState() {
      if (buttonSend != null) {
        buttonSend.setEnabled((textPage != null) && (textPage.isModified()));
      }
    }

    /**
     * Action called when a page is selected (after page is loaded).
     */
    void actionPageSelected() {
      if (page != null) {
        textPage.setText(page.getContents());
        textPage.setModified(false);
        ArrayList<CheckErrorAlgorithm> errorsFound = CheckError.analyzeErrors(
            errors, page, textPage.getText());
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
      }
    }

    /**
     * Action called when an error is selected. 
     */
    void actionSelectError() {
      Object selection = listErrors.getSelectedValue();
      if (selection instanceof CheckErrorAlgorithm) {
        CheckErrorAlgorithm algorithm = (CheckErrorAlgorithm) selection;
        boolean modified = textPage.isModified();
        String contents = textPage.getText();
        ArrayList<CheckErrorResult> errorsFound = CheckError.analyzeError(
            algorithm, page, contents);
        boolean visible = false;
        textPage.resetAttributes();
        StyledDocument document = textPage.getStyledDocument();
        if (document != null) {
          if (errorsFound != null) {
            for (CheckErrorResult errorFound : errorsFound) {
              document.setCharacterAttributes(
                  errorFound.getStartPosition(),
                  errorFound.getLength(),
                  textPage.getStyle(MediaWikiConstants.STYLE_DISAMBIGUATION_LINK),
                  true);
              SimpleAttributeSet attributes = new SimpleAttributeSet();
              attributes.addAttribute(MediaWikiConstants.ATTRIBUTE_INFO, errorFound);
              document.setCharacterAttributes(
                  errorFound.getStartPosition(),
                  errorFound.getLength(),
                  attributes, false);
              if (!visible) {
                textPage.setCaretPosition(errorFound.getStartPosition());
                textPage.moveCaretPosition(errorFound.getEndPosition());
                visible = true;
              }
            }
          }
        }
        textPage.setModified(modified);
        updateComponentState();
      }
    }

    /* (non-Javadoc)
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent e) {
      if (e == null) {
        return;
      }

      if (ACTION_FULL_ANALYSIS_PAGE.equals(e.getActionCommand())) {
        Controller.runFullAnalysis(page.getTitle(), null, getWikipedia());
      } else if (ACTION_MARK_AS_FIXED.equals(e.getActionCommand())) {
        actionMarkAsFixed();
      } else if (ACTION_SEND.equals(e.getActionCommand())) {
        actionSend();
      } else if (ACTION_VALIDATE.equals(e.getActionCommand())) {
        actionValidate();
      } else if (ACTION_VIEW.equals(e.getActionCommand())) {
        actionView();
      }
    }

    /**
     * Mark a page as fixed. 
     */
    private void actionMarkAsFixed() {
      ArrayList<CheckErrorResult> results = CheckError.analyzeError(
          error.getAlgorithm(), page, page.getContents());
      if ((results != null) && (!results.isEmpty())) {
        displayWarning(GT._(
            "The error n°{0} is still found {1} times in the page.\n" +
            "You must fix them and save the page before marking it as fixed.",
            new Object[] { Integer.toString(error.getErrorNumber()), results.size() } ));
        return;
      }
      if (displayYesNoWarning(
          GT._("Do you want to mark {0} as fixed for error n°{1}",
               new Object[] { page.getTitle(), Integer.toString(error.getErrorNumber())})) == JOptionPane.YES_OPTION) {
        error.fix(page);
        actionSelectErrorType();
      }
    }

    /**
     * Compute comment.
     * 
     * @param errorsFixed Errors fixed
     * @return Comment.
     */
    private String getComment(ArrayList<CheckErrorAlgorithm> errorsFixed) {
      StringBuilder comment = new StringBuilder(getDefaultComment());
      if (errorsFixed != null) {
        for (int pos = 0; pos < errorsFixed.size(); pos++) {
          if (pos > 0) {
            comment.append(", ");
          } else {
            comment.append(": ");
          }
          //comment.append("[http://toolserver.org/~sk/cgi-bin/checkwiki/checkwiki.cgi?project=");
          //comment.append(getWikipedia().getCode());
          //comment.append("wiki&view=only&id=");
          //comment.append(errorsFixed.get(pos).getErrorNumber());
          //comment.append(" ");
          comment.append(errorsFixed.get(pos).getErrorDescription());
          //comment.append("]");
        }
      }
      return comment.toString();
    }

    /**
     * @return Errors fixed.
     */
    private ArrayList<CheckErrorAlgorithm> computeErrorsFixed() {
      final ArrayList<CheckErrorAlgorithm> errorsFixed = new ArrayList<CheckErrorAlgorithm>();
      for (int pos = 0; pos < modelErrors.size(); pos++) {
        if (modelErrors.get(pos) instanceof CheckErrorAlgorithm) {
          CheckErrorAlgorithm initialAlgorithm = (CheckErrorAlgorithm) modelErrors.get(pos);
          ArrayList<CheckErrorResult> results = CheckError.analyzeError(
              initialAlgorithm, page, textPage.getText());
          if ((results == null) || (results.isEmpty())) {
            errorsFixed.add(initialAlgorithm);
          }
        }
      }
      return errorsFixed;
    }

    /**
     * Send page.
     */
    private void actionSend() {
      // Check page text to see what errors are still present
      final ArrayList<CheckErrorAlgorithm> errorsFixed = computeErrorsFixed();
      updateComment(errorsFixed);

      // Send page
      SendWorker sendWorker = new SendWorker(
          getWikipedia(), CheckWikiProjectWindow.this,
          page, textPage.getText(), textComment.getText());
      sendWorker.setListener(new DefaultBasicWorkerListener() {
        @Override
        public void afterFinished(
            @SuppressWarnings("unused") BasicWorker worker,
            boolean ok) {
          if (ok) {
            // Close pane
            pane.remove(CheckWikiContentPanel.this);

            // Remove errors fixed
            for (int posError = 0; posError < listAllErrors.getModel().getSize(); posError++) {
              Object element = listAllErrors.getModel().getElementAt(posError);
              if (element instanceof CheckError) {
                final CheckError tmpError = (CheckError) element;
                for (int posAlgo = 0; posAlgo < errorsFixed.size(); posAlgo++) {
                  if (tmpError.getAlgorithm().equals(errorsFixed.get(posAlgo))) {
                    tmpError.remove(page);
                    MediaWikiController.addSimpleTask(new Callable<Page>() {

                      public Page call() throws Exception
                      {
                        tmpError.fix(page);
                        return page;
                      }});
                  }
                }
              }
            }
            actionSelectErrorType();
          }
        }
      });
      sendWorker.start();
    }

    /**
     * Validate current text and recompute errors.
     */
    private void actionValidate() {
      actionSelectError();
      updateComment(null);
    }

    /**
     * View page in external viewer.
     */
    private void actionView() {
      Utilities.browseURL(getWikipedia(), page.getTitle());
    }

    /**
     * Update automatic comment.
     * 
     * @param errorsFixed Errors.
     */
    private void updateComment(ArrayList<CheckErrorAlgorithm> errorsFixed) {
      if ((chkAutomaticComment != null) &&
          (textComment != null)) {
        textComment.setEditable(!chkAutomaticComment.isSelected());
        if (chkAutomaticComment.isSelected()) {
          textComment.setText(getComment((errorsFixed != null) ? errorsFixed : computeErrorsFixed()));
        }
      }
    }

    /* (non-Javadoc)
     * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
     */
    public void itemStateChanged(ItemEvent e) {
      if ((e == null) || (e.getSource() == null)) {
        return;
      }
      Object source = e.getSource();
      if ((source == chkAutomaticComment)) {
        updateComment(null);
      }
    }
  }

  /**
   * @return Default comment.
   */
  @Override
  protected String getDefaultComment() {
    return GT._("Detection by [[" + getWikipedia().getCheckWikiProject() + "]]");
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
    //String contents = projectPage.getContents();
    //errors = CheckError.initCheckErrors(getWikipedia(), contents);
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
      int nbPages = error.getPageCount();
      for (int numPage = 0; numPage < nbPages; numPage++) {
        Page page = error.getPage(numPage);
        modelPages.addElement(page);
      }
      setPageLoaded(false);
      actionSelectPage();
    }
  }

  /**
   * A close icon for JTabbedPane.
   */
  private static class CloseIcon implements Icon {

    private final static int SIZE = 10;
    transient Rectangle position = null; 

    /**
     * @param pane Pane component.
     * @param component Page.
     */
    public CloseIcon(final JTabbedPane pane, final Component component) {
      MouseAdapter adapter = new MouseAdapter() {
        @Override
        public void mouseClicked(MouseEvent e) {
          if (!e.isConsumed() &&
              (position != null) &&
              position.contains(e.getX(), e.getY())) {
            for (int i = pane.getComponentCount(); i > 0; i--) {
              if (pane.getComponent(i - 1) == component) {
                pane.remove(i - 1);
                pane.removeMouseListener(this);
              }
            }
          }
        }
      };
      pane.addMouseListener(adapter);
    }

    public int getIconHeight() {
      return SIZE;
    }

    public int getIconWidth() {
      return SIZE;
    }

    /* (non-Javadoc)
     * @see javax.swing.Icon#paintIcon(java.awt.Component, java.awt.Graphics, int, int)
     */
    public void paintIcon(@SuppressWarnings("unused") Component c, Graphics g, int x, int y) {
      if (g instanceof Graphics2D) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.draw3DRect(x, y, getIconWidth() - 1, getIconHeight() - 1, false);
        g2.drawLine(x + 2, y + 2, x + getIconWidth() - 3, y + getIconHeight() - 3);
        g2.drawLine(x + 2, y + getIconHeight() - 3, x + getIconWidth() - 3, y + 2);
        g2.dispose();
      }
      position = new Rectangle(x, y, getIconWidth(), getIconHeight());
    }
    
  }

  /**
   * Action called when a page is selected.
   */
  void actionSelectPage() {
    Object selection = listPages.getSelectedValue();
    if (selection instanceof Page) {
      Page pageSelected = (Page) selection;
      final CheckWikiContentPanel contentPanel = createContentsComponents(
          contentPane, pageSelected,
          (CheckError) modelAllErrors.getSelectedItem());
      contentPane.add(contentPanel);
      contentPane.setIconAt(contentPane.getComponentCount() - 1, new CloseIcon(contentPane, contentPanel));
      contentPane.setSelectedComponent(contentPanel);
      RetrieveContentWorker contentWorker = new RetrieveContentWorker(getWikipedia(), this, pageSelected);
      contentWorker.setListener(new DefaultBasicWorkerListener() {

        /* (non-Javadoc)
         * @see org.wikipediacleaner.gui.swing.basic.DefaultBasicWorkerListener#beforeFinished(org.wikipediacleaner.gui.swing.basic.BasicWorker)
         */
        @Override
        public void beforeFinished(BasicWorker worker) {
          super.beforeFinished(worker);
          contentPanel.actionPageSelected();
        }
        //
      });
      contentWorker.start();
    } else {
      updateComponentState();
    }
  }

  /**
   * Action called when Reload button is pressed. 
   */
  @Override
  protected void actionReload() {
    clean();
    contentPane.removeAll();
    errors = new ArrayList<CheckError>();
    CheckWikiProjectWorker reloadWorker = new CheckWikiProjectWorker(
        getWikipedia(), this, errors);
    setupReloadWorker(reloadWorker);
    reloadWorker.start();
  }
}
