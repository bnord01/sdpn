package de.wwu.sdpn.util.proxies.dataflow;

import scala.collection.Set;

import com.ibm.wala.fixpoint.AbstractVariable;

public abstract class SetVariableProxy<T> extends AbstractVariable <SetVariableProxy<T>>{
	public abstract Set<T> getSet();
	public abstract void setSet(Set<T> set);
}
