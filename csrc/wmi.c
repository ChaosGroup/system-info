#define _WIN32_DCOM
#include <Wbemidl.h>
#include <stdbool.h>


HRESULT init() {
  HRESULT hres = CoInitializeEx(0, COINIT_MULTITHREADED);

  if (!FAILED(hres)) {
    hres = CoInitializeSecurity(
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
  return hres;
}

HRESULT createInstance(IWbemLocator* plocator) {
  return CoCreateInstance(
    &CLSID_WbemLocator,
    0,
    CLSCTX_INPROC_SERVER,
    &IID_IWbemLocator, (LPVOID *)&plocator);
}

HRESULT connectServer(IWbemLocator* plocator, IWbemServices* pservices) {
  if (!plocator) return E_INVALIDARG;

  HRESULT hres = plocator->lpVtbl->ConnectServer(
    plocator,
    SysAllocString(L"ROOT\\CIMV2"), // Object path of WMI namespace
    NULL,                           // User name. NULL = current user
    NULL,                           // User password. NULL = current
    NULL,                           // Locale. NULL indicates current
    0,                              // Security flags.
    NULL,                           // Authority (for example, Kerberos)
    NULL,                           // Context object
    &pservices);

  if (!FAILED(hres)) {
    hres = CoSetProxyBlanket(
      (IUnknown *)pservices,
      RPC_C_AUTHN_WINNT,           // RPC_C_AUTHN_xxx
      RPC_C_AUTHZ_NONE,            // RPC_C_AUTHZ_xxx
      NULL,                        // Server principal name
      RPC_C_AUTHN_LEVEL_CALL,      // RPC_C_AUTHN_LEVEL_xxx
      RPC_C_IMP_LEVEL_IMPERSONATE, // RPC_C_IMP_LEVEL_xxx
      NULL,                        // client identity
      EOAC_NONE);                  // proxy capabilities

    if (FAILED(hres)) {
      pservices->lpVtbl->Release(pservices);
      pservices = NULL;
    }
  }

  return hres;
}

HRESULT query(IWbemServices* pservices, BSTR query, IEnumWbemClassObject* penumerator) {
  if (!pservices) return E_INVALIDARG;

  return pservices->lpVtbl->ExecQuery(
    pservices,
    SysAllocString(L"WQL"),
    query,
    WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY,
    NULL,
    &penumerator);
}

HRESULT getStringField(IEnumWbemClassObject* penumerator, BSTR field, wchar_t* value) {
  if (!penumerator) return E_INVALIDARG;

  IWbemClassObject *pclassObject = NULL;
  ULONG ures = 0;
  HRESULT hres = penumerator->lpVtbl->Next(penumerator, WBEM_INFINITE, 1, &pclassObject, &ures);

  if (!FAILED(hres) && ures) {
    VARIANT vProperty;
    hres = pclassObject->lpVtbl->Get(pclassObject, field, 0, &vProperty, 0, 0);

    if (!FAILED(hres)) {
      value = vProperty.bstrVal;
      VariantClear(&vProperty);
    }
    pclassObject->lpVtbl->Release(pclassObject);
  }

  return hres;
}

HRESULT getOS(wchar_t* os) {
  HRESULT hres = init();
  if (FAILED(hres)) {
    CoUninitialize();
    return hres;
  }

  IWbemLocator* plocator = NULL;
  hres = createInstance(plocator);
  if (FAILED(hres)) {
    CoUninitialize();
    return hres;
  }

  IWbemServices* pservices = NULL;
  hres = connectServer(plocator, pservices);
  if (FAILED(hres)) {
    pservices->lpVtbl->Release(pservices);
    plocator->lpVtbl->Release(plocator);
    CoUninitialize();
    return hres;
  }

  IEnumWbemClassObject* penumerator = NULL;
  hres = query(pservices, SysAllocString(L"SELECT * FROM Win32_OperatingSystem"), penumerator);
  if (FAILED(hres)) {
    penumerator->lpVtbl->Release(penumerator);
    pservices->lpVtbl->Release(pservices);
    plocator->lpVtbl->Release(plocator);
    CoUninitialize();
    return hres;
  }

  hres = getStringField(penumerator, SysAllocString(L"Caption"), os);

  penumerator->lpVtbl->Release(penumerator);
  pservices->lpVtbl->Release(pservices);
  plocator->lpVtbl->Release(plocator);
  CoUninitialize();

  return hres;
}
