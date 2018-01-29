#define _WIN32_DCOM
#include <Wbemidl.h>


void init(HRESULT* hres) {
  *hres = CoInitializeEx(0, COINIT_MULTITHREADED);
  if (FAILED(hres)) return;

  *hres = CoInitializeSecurity(
    NULL,
    -1,
    NULL,
    NULL,
    RPC_C_AUTHN_LEVEL_DEFAULT,
    RPC_C_IMP_LEVEL_IMPERSONATE,
    NULL,
    EOAC_NONE,
    NULL);
}

IWbemLocator* createInstance(HRESULT* hres) {
  IWbemLocator *plocator = NULL;
  *hres = CoCreateInstance(
    &CLSID_WbemLocator,
    0,
    CLSCTX_INPROC_SERVER,
    &IID_IWbemLocator, (LPVOID *)&plocator);

  if (FAILED(*hres)) {
    plocator = NULL;
  }
  return plocator;
}

IWbemServices* connectServer(HRESULT* hres, IWbemLocator *plocator) {
  IWbemServices *pservices = NULL;
  *hres = plocator->lpVtbl->ConnectServer(
    plocator,
    SysAllocString(L"ROOT\\CIMV2"), // Object path of WMI namespace
    NULL,                           // User name. NULL = current user
    NULL,                           // User password. NULL = current
    NULL,                           // Locale. NULL indicates current
    0,                              // Security flags.
    NULL,                           // Authority (for example, Kerberos)
    NULL,                           // Context object
    &pservices);

  if (FAILED(*hres)) {
    return NULL;
  }

  *hres = CoSetProxyBlanket(
    (IUnknown *)pservices,
    RPC_C_AUTHN_WINNT,           // RPC_C_AUTHN_xxx
    RPC_C_AUTHZ_NONE,            // RPC_C_AUTHZ_xxx
    NULL,                        // Server principal name
    RPC_C_AUTHN_LEVEL_CALL,      // RPC_C_AUTHN_LEVEL_xxx
    RPC_C_IMP_LEVEL_IMPERSONATE, // RPC_C_IMP_LEVEL_xxx
    NULL,                        // client identity
    EOAC_NONE);                  // proxy capabilities

  if (FAILED(*hres)) {
    pservices->lpVtbl->Release(pservices);
    pservices = NULL;
  }

  return pservices;
}

IEnumWbemClassObject* query(HRESULT* hres, IWbemServices* pservices, BSTR query) {
  IEnumWbemClassObject* penumerator = NULL;
  *hres = pservices->lpVtbl->ExecQuery(
    pservices,
    SysAllocString(L"WQL"),
    query,
    WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY,
    NULL,
    &penumerator);

  if (FAILED(*hres)) {
    penumerator = NULL;
  }

  return penumerator;
}

wchar_t* getStringField(HRESULT* hres, IEnumWbemClassObject* penumerator, BSTR field) {
  wchar_t* res = L"";
  IWbemClassObject *pclassObject = NULL;
  ULONG ures = 0;
  *hres = penumerator->lpVtbl->Next(penumerator, WBEM_INFINITE, 1, &pclassObject, &ures);
  if (FAILED(*hres) || ures == 0) {
    return res;
  }

  VARIANT vProperty;
  *hres = pclassObject->lpVtbl->Get(pclassObject, field, 0, &vProperty, 0, 0);
  if (FAILED(*hres)) {
    pclassObject->lpVtbl->Release(pclassObject);
    return res;
  }

  res = vProperty.bstrVal;
  VariantClear(&vProperty);
  pclassObject->lpVtbl->Release(pclassObject);
  return res;
}

wchar_t* getOS(HRESULT* hres) {
  wchar_t* res = L"";

  init(hres);
  if (FAILED(*hres)) {
    CoUninitialize();
    return res;
  }

  IWbemLocator *plocator = createInstance(hres);
  if (FAILED(*hres)) {
    CoUninitialize();
    return res;
  }

  IWbemServices *pservices = connectServer(hres, plocator);
  if (FAILED(*hres)) {
    pservices->lpVtbl->Release(pservices);
    plocator->lpVtbl->Release(plocator);
    CoUninitialize();
    return res;
  }

  IEnumWbemClassObject* penumerator = query(hres,
                                            pservices,
                                            SysAllocString(L"SELECT * FROM Win32_OperatingSystem"));
  if (FAILED(*hres)) {
    penumerator->lpVtbl->Release(penumerator);
    pservices->lpVtbl->Release(pservices);
    plocator->lpVtbl->Release(plocator);
    CoUninitialize();
    return res;
  }

  res = getStringField(hres, penumerator, SysAllocString(L"Caption"));
  if (FAILED(*hres)) {
    res = L"";
  }

  penumerator->lpVtbl->Release(penumerator);
  pservices->lpVtbl->Release(pservices);
  plocator->lpVtbl->Release(plocator);
  CoUninitialize();
  return res;
}
