package ops.market.common.annocation;
/**
 * 系统内置定义的名字种类。
 * @author lant
 *
 */
public enum  SSNameKind
{
   /**
    *  
    *  NormalName 正规名称.
    *  FullPathName,全路径名.
    *  AliasName,别名
    *  BRCode,工商注册码
    *  BRName，工商注册名，一般用于发票名称。
    *  BRAddress 工商注册地址。
    *  BankAccCode 银行账户码
    *  BnakAccName 银行账户名
    *  PostalCode  邮编
    *  PostalAddress 邮寄地址
    *  StreetAddress 街道地址
    *  TownDetail 城市名称
    *  ElectronicAddress 电子邮件地址
    *  TelephoneNumber 固定电话
    *  MobilephoneNumber 移动电话
    */
  NormalName,FullPathName,AliasName,BRCode,BRName,
  BRAddress,BankCode,BnakName,PostalCode,PostalAddress,StreetAddress,
  TownDetail,FaxAddress,ElectronicAddress,TelephoneNumber,MobilephoneNumber
} 