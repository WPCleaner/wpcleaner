/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.io.Externalizable;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;


/**
 * A generic composite comparator class.
 * 
 * A list of several simple comparators are used one after
 * an other until a meaningful result is obtained.
 * @param <T>
 */
public class CompositeComparator<T> implements NamedComparator<T>, Cloneable, Externalizable {

  private List<NamedComparator<T>> comparators;
  private List<Boolean> orders;
  private final String name;

  /**
   * Composite comparator constructor.
   */
  public CompositeComparator() {
    this(null, null, null);
  }

  /**
   * Composite comparator constructor.
   * 
   * @param name Comparator name.
   */
  public CompositeComparator(String name) {
    this(name, null, null);
  }

  /**
   * @param name Comparator name.
   * @param comparators List of comparators.
   */
  public CompositeComparator(
      String name,
      List<NamedComparator<T>> comparators) {
    this(name, comparators, null);
  }

  /**
   * @param name Comparator name.
   * @param comparators List of comparators.
   * @param orders List of orders.
   */
  public CompositeComparator(
      String name,
      List<NamedComparator<T>> comparators,
      List<Boolean> orders) {
    this.name = name;
    setComparators(comparators);
    setOrders(orders);
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.data.NamedComparator#getName()
   */
  @Override
  public String getName() {
    return name;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return name;
  }

  /**
   * @param comparators List of comparators.
   */
  public void setComparators(List<NamedComparator<T>> comparators) {
    this.comparators = comparators;
  }

  /**
   * @return Number of comparators.
   */
  public int getComparatorsCount() {
    return (comparators != null) ? comparators.size() : 0;
  }

  /**
   * @param index Comparator index.
   * @return Comparator.
   */
  public NamedComparator<T> getComparator(int index) {
    return comparators.get(index);
  }

  /**
   * @param orders List of orders.
   */
  public void setOrders(List<Boolean> orders) {
    this.orders = orders;
  }

  /**
   * @param index Comparator index.
   * @return Comparator order (TRUE = Ascending).
   */
  public Boolean getOrder(int index) {
    if ((orders != null) && (index >= 0) && (index < orders.size())) {
      return orders.get(index);
    }
    return Boolean.TRUE;
  }

  /**
   * @param itemName Comparator name.
   * @param up Flag indicating if the comparator should be moved up or down.
   */
  public void moveComparator(String itemName, boolean up) {
    for (int i = 0; i < comparators.size(); i++) {
      NamedComparator<T> comparator = comparators.get(i);
      if (comparator.getName().equals(itemName)) {
        if (up) {
          if (i > 0) {
            comparators.remove(i);
            comparators.add(i - 1, comparator);
            if ((orders != null) && (orders.size() >= i)) {
              if (orders.size() == i) {
                orders.add(i - 1, Boolean.TRUE);
              } else {
                Boolean old = orders.remove(i);
                orders.add(i - 1, old);
              }
            }
          }
        } else {
          if (i < comparators.size() - 1) {
            comparators.remove(i);
            comparators.add(i + 1, comparator);
            if ((orders != null) && (orders.size() > i)) {
              if (orders.size() == i + 1) {
                orders.add(i, Boolean.TRUE);
              } else {
                Boolean old = orders.remove(i);
                orders.add(i + 1, old);
              }
            }
          }
        }
        return;
      }
    }
  }

  /**
   * Reorder comparators.
   * 
   * @param names Elementary comparator names.
   * @param newOrders New orders.
   */
  public void orderComparators(List<String> names, List<Boolean> newOrders) {
    if (comparators != null) {
      for (int index = names.size(); index > 0; index--) {
        String comparatorName = names.get(index);
        for (int i = 0; i < comparators.size(); i++) {
          NamedComparator<T> comparator = comparators.get(i);
          if (comparatorName.equals(comparator.getName())) {
            comparators.remove(i);
            comparators.add(0, comparator);
            if (orders.size() > i) {
              orders.remove(i);
            }
            orders.add(0, newOrders.get(index));
          }
        }
      }
    }
  }

  /* (non-Javadoc)
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  @Override
  public int compare(T o1, T o2) {
    if (comparators != null) {
      Iterator<NamedComparator<T>> itComparator = comparators.iterator();
      Iterator<Boolean> itBoolean = (orders != null) ? orders.iterator() : null;
      while (itComparator.hasNext()) {
        Comparator<T> c = itComparator.next();
        Boolean order = ((itBoolean != null) && (itBoolean.hasNext())) ? itBoolean.next() : Boolean.TRUE;
        int result = c.compare(o1, o2);
        //System.out.println("Comparing [" + o1 + "] and [" + o2 + "] by " + c + ": " + result);
        if (result != 0) {
          return (Boolean.FALSE.equals(order) ? -1 : 1) * result;
        }
      }
    }
    return 0;
  }

  /* (non-Javadoc)
   * @see java.io.Externalizable#readExternal(java.io.ObjectInput)
   */
  @Override
  public void readExternal(@SuppressWarnings("unused") ObjectInput input) {
    //
  }

  /* (non-Javadoc)
   * @see java.io.Externalizable#writeExternal(java.io.ObjectOutput)
   */
  @Override
  public void writeExternal(@SuppressWarnings("unused") ObjectOutput output) {
    //
  }

  /* (non-Javadoc)
   * @see java.lang.Object#clone()
   */
  @SuppressWarnings("unchecked")
  @Override
  public CompositeComparator<T> clone() {
    try {
      return (CompositeComparator<T>) super.clone();
    } catch (CloneNotSupportedException e) {
      //
    }
    return null;
  }
}
